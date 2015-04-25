# ***********************************************
# ***********************************************
# Author: Benjamin Tovar
# Date: April 25, 2015
# Post: http://btovar.com/2015/04/text-mining-in-r/ ‎
# ***********************************************
# ***********************************************

library(reshape)
library(ggplot2)
library(tm)
library(wordcloud)

# ****************************
# load data and labels
# ***************************

# set input files
dataset_file <- "dataset.txt"
labels_file <- "labels.txt"
# read file 
dataset <- read.delim(dataset_file,header=FALSE)
# read labels
dataset_labels <- read.delim(labels_file,header=FALSE)
dataset_labels <- dataset_labels[,1]
# add word "class_" for each label
dataset_labels_p <- paste("class",dataset_labels,sep="_")

# ****************************
# print and plot the number 
# of samples for each class
# ***************************

# > table(dataset_labels_p)
# dataset_labels_p
# class_1 class_2 class_3 class_4 class_5 class_6 class_7 class_8 
#    2840    1596     251     108      41     253     190     206 
pdf("barplot_sample_number_per_class.pdf",height=6,width=8)
  ggplot(as.data.frame(table(dataset_labels_p))) + 
    aes(x=dataset_labels_p,y=Freq) + 
    geom_bar(stat="identity",aes(fill=dataset_labels_p),alpha=0.7) + 
    theme(legend.position="none") +
    ylim(0,3000) +  
    labs(title="Sample number for each document class",
        x="Class",y="Number of samples")
dev.off()

# ****************************
# Pre-process data
# ***************************

# get unique class name list
unique_labels <- unique(dataset_labels_p)
# merge documents that match 
# certain class into a list object
dataset_s <- sapply(unique_labels,function(label) list( dataset[dataset_labels_p %in% label,1] ) )
# double check that we are including each document into each list index
as.data.frame(lapply(dataset_s,length))
#   class_1 class_2 class_3 class_4 class_5 class_6 class_7 class_8
# 1    2840    1596     251     108      41     253     190     206

# ****************************
# Compute document corpus 
# to make "text mining"
# ***************************

# convert each list content into a corpus
dataset_corpus <- lapply(dataset_s, function(x) Corpus(VectorSource( toString(x) ))) 
# merge all documents into one single corpus
dataset_corpus_all <- dataset_corpus[[1]]
for (i in 2:length(unique_labels)) {
  dataset_corpus_all <- c(dataset_corpus_all,dataset_corpus[[i]])
}

# > dataset_corpus_all
# <<VCorpus (documents: 8, metadata (corpus/indexed): 0/0)>>
# > summary(dataset_corpus_all)
#   Length Class             Mode
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list
# 1 2      PlainTextDocument list

# ****************************
# text pre-processing
# ***************************

# remove punctuation
dataset_corpus_all <- tm_map(dataset_corpus_all, removePunctuation)
# remove numbers
dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
# remove stopwords
dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x,stopwords("english")))

# set a list of words to remove 
# which I found out appears many times 
# in all documents, but does not say "much"
# about document class.
# feel free to comment next lines
words_to_remove <- c("said","from","what","told","over","more","other","have","last",
                    "with","this","that","such","when","been","says","will","also",
                     "where","why","would","today")
dataset_corpus_all <- tm_map(dataset_corpus_all, removeWords, words_to_remove)


# ****************************
# compute term matrices M[i,j]
# where:
# [i,j] <- frequency of word i in document class j
# ***************************

# compute term matrix
document_tm <- TermDocumentMatrix(dataset_corpus_all)
# convert to matrix class
document_tm_mat <- as.matrix(document_tm)
colnames(document_tm_mat) <- unique_labels
# compute dimensions of matrix
dim(document_tm_mat)

# ****************************
# For quality control, remove 
# sparse terms
# ***************************

# FROM: ?removeSparseTerms
#
# Usage:
#
#      removeSparseTerms(x, sparse)
#    
# Arguments:
#
#        x: A ‘DocumentTermMatrix’ or a ‘TermDocumentMatrix’.
#
#   sparse: A numeric for the maximal allowed sparsity in the range from
#           bigger zero to smaller one.
#
# Value:
#
#      A term-document matrix where those terms from ‘x’ are removed
#      which have at least a ‘sparse’ percentage of empty (i.e., terms
#      occurring 0 times in a document) elements. I.e., the resulting
#      matrix contains only terms with a sparse factor of less than
#      ‘sparse’.


document_tm_clean <- removeSparseTerms(document_tm, 0.8)
# convert to matrix class
document_tm_clean_mat <- as.matrix(document_tm_clean)
colnames(document_tm_clean_mat) <- unique_labels
# compute dimensions of matrix
dim(document_tm_clean_mat)

# ****************************
# For extra quality control
# remove words in term matrix
# with length < 4
# ***************************

# compute row index
index <- as.logical(sapply(rownames(document_tm_clean_mat), function(x) (nchar(x)>3) ))
# extract new term matrix
document_tm_clean_mat_s <- document_tm_clean_mat[index,]
# compute dimensions of matrix
dim(document_tm_clean_mat_s)

# ****************************
# compute comparison 
# word clouds
# ***************************

# FROM: ?comparison.cloud
# 
#      Let p_{i,j} be the rate at which word i occurs in document j, and
#      p_j be the average across documents(sum_ip_{i,j}/ndocs). The size
#      of each word is mapped to its maximum deviation (
#      max_i(p_{i,j}-p_j) ), and its angular position is determined by
#      the document where that maximum occurs.

# first top 500 discriminant words
pdf("comparison_cloud_top_500_words.pdf",height=8,width=8)
  comparison.cloud(document_tm_clean_mat_s,
                  max.words=500,
                  random.order=FALSE,c(4,0.4),
                  title.size=1.4)
dev.off()

# first top 2000 discriminant words
pdf("comparison_cloud_top_2000_words.pdf",height=8,width=8)
  comparison.cloud(document_tm_clean_mat_s,
                  max.words=2000,
                  random.order=FALSE,c(4,0.4),
                  title.size=1.4)
dev.off()

# ****************************
# compute commonality 
# word clouds
# ***************************

# FROM: ?commonality.cloud
#   Plot a cloud of words shared across documents

# first top 2000 common words across classes
pdf("commonality_cloud_top_2000_words.pdf",height=5,width=5)
  commonality.cloud(document_tm_clean_mat_s,
                  max.words=2000,
                  random.order=FALSE)
dev.off()

# ****************************
# Plot adjusted word frequency 
# for each dataset
# ***************************

# NOTE: the adjustment means that:
# word with an adjusted value = 1.0, indicates that
# is the most frequent word in document class. 
# The remaining values are adjusted 
# in function of the original frequency of top word.

# adjust by max value found for each document class
document_tm_clean_mat_s_norm <- apply(document_tm_clean_mat_s,2,function(col) col/max(col) )
# > colSums(document_tm_clean_mat_s_norm)
#  class_1  class_2  class_3  class_4  class_5  class_6  class_7  class_8 
# 24.21316 22.87277 30.76670 29.17125 27.56000 27.43659 32.27465 33.85191 

dataset_wf <- melt(document_tm_clean_mat_s_norm)
dataset_wf <- cbind(dataset_wf,
                  # add colors depending on the score      
                  category=ifelse(dataset_wf$value<=0.25,"0.25", 
                            ifelse(dataset_wf$value<=0.5, "0.5", 
                              ifelse(dataset_wf$value<=0.75, "0.75", 
                                ifelse(dataset_wf$value<=1, "1.0","lol"))))
                  )

# > dataset_wf[1:5,]
#       Terms    Docs       value category
# 1   abandon class_1 0.000000000  0.25
# 2 abandoned class_1 0.000483910  0.25
# 3     abbey class_1 0.000483910  0.25
# 4     abdel class_1 0.000000000  0.25
# 5     abdul class_1 0.000241955  0.25

# ***************************
# count the number of words 
# per category
# ***************************

# > table(dataset_wf$category)
#  0.25   0.5  0.75   1.0 
# 58108    74    16    18 

# Given that we have 58108 words in 0.25 category.
# it might make sense to "pick" randomly only a sample
# of the full category to help with the visualization

# extract all non 0.25 items
index <- !dataset_wf$category %in% "0.25"
dataset_wf2 <- dataset_wf[index,]
# rbind with 500 samples of 0.25 category items
index <- sample(rownames(dataset_wf[dataset_wf$category %in% "0.25",]),
                500,replace=FALSE)
dataset_wf2 <- rbind(dataset_wf2,dataset_wf[index,])

# > table(dataset_wf2$category)
# 0.25  0.5 0.75  1.0 
# 500   74   16   18 

pdf("adjusted_wf.pdf",width=8,height=6)
  ggplot(dataset_wf2) +
    aes(x=Docs,y=value) +
    geom_text(aes(label=Terms,size=1,colour=category)) + 
    labs(title="Adjusted frequency of words for each dataset",
        x="Document class",y="Adjusted word frequency") + 
    theme_minimal() +
    theme(legend.position="none") 
dev.off()


