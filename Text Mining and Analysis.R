#Import dataset
library(readr)
TISS_reviews <- read_csv("C:/Users/user/Desktop/reviews.csv")

#view dataset
View(TISS_reviews)

#dimension of dataset
dim(TISS_reviews)

#change the limit of the printing by using max.print option.
options(max.print = 100000)


#nchar will return the number of characters in a string.
nchar(TISS_reviews$Review)

#Average number of characters 
sum(nchar(TISS_reviews$Review))/304

#________________________________________________________________________________________________________

#
#text-preprocessing functions from tm package.
library(tm)

#The five most common tasks to be performed are 
# 1.	lowering text, 
# 2.	removing punctuation, 
# 3.	stripping extra whitespace, 
# 4.	removing numbers and 
# 5.	removing "stopwords" - Stopwords are common words that often do not provide any additional insight.

getTransformations()

stopwords('english')

#Customised stopwords
custom.stopwords <- c(stopwords('english'), 'tata', 'tiss', 'institute')

#Apply various preprocessing functions.

#For tolower function, you have to add an additional argument, 
#"content_transformer" because you are using a customized version of "tolower" which modifies the content of an R object.

#tm_map() takes two arguments, a corpus and a cleaning function.

#In the clean_corpus function, the order of operations is important. 

#tm_map a transformation function taking a text document as input and returning a text document. 
#The function content_transformer can be used to create a wrapper to get and set the content of text documents.

clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)}

#Define the reviews object as your corpus or collection of natural language documents.
#Make a vector source: Tiss . 
#A vector source interprets each element or each row as a document.
TISS <- VectorSource(TISS_reviews[,2])

#Make a volatile corpus: TISScorpus.
#Corpus is the main structure that tm uses for storing and manipulating text documents. 
#There are two types VCorpus (Volatile Corpus) and PCorpus (Permanent Corpus).
#A volatile corpus is fully kept in memory and thus all changes only affect the corresponding R object.
TISScorpus <- VCorpus(TISS)
TISScorpus

#Apply text cleaning in Volatile corpus.
reviews_clean<-clean.corpus(TISScorpus)
reviews_clean

#TermDocumentMatrix: bag of words text mining methodology
reviews_tdm<-TermDocumentMatrix(reviews_clean)
View(reviews_tdm)


#Reclassify TDM as a matrix
reviews <- as.matrix(reviews_tdm)
View(reviews)
dim(reviews) 
#First check the dimensions of the data frame. 
#You will notice that it is 1 column and 1108 rows.
#This means that in total there are 1108 distinct words after cleaning steps.

#Word frequency 
#Sum across each row because each row is a unique word in the corpus.
#You need to call the base R function rowSums and pass the matrix to it.
word.freq<-rowSums(reviews)

#Create a new data frame object by putting the original words and the term frequencies next to each other. The new freq.df object has two columns, one for terms and another that is just the summation for each row.
freq.df<-data.frame(word = names(word.freq), frequency = word.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

#Now you can sort the data frame and then look at the first ten most frequent terms. 
freq.df[1:10,] 

#__________________________________________________

#Word Clouds. A word cloud is a visualization based on frequency. In a word cloud, words are represented with varying font size.
#The code snippet will create a word cloud with a maximum number of terms equal to 100. You can also specify the minimum frequency threshold as well. The code also specifies blue and darkred colours (color words from least to most frequent). You can change the colors manually. 
m <- as.matrix(reviews)
View(m)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq = v)
View(d)

#Display the TISS word Cloud
library(wordcloud2)
wordcloud2(d)
