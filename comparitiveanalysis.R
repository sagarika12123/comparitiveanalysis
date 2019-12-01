#load package for text mining
library(tm)
#read in the documents and create a corpus
documents <- Corpus(DirSource('/Users/sagarikasuresh/Desktop/untitled folder'))
inspect(documents)
View(documents)
#inspect particular documents
writeLines(as.character(documents[[1]]))
##data preprocessing, most tedious and time consuming but increases the efficiency of the code
hispace <- content_transformer(function(x,pattern) { return (gsub(pattern," ",x))})
documents <- tm_map(documents,hispace,"-")
documents <- tm_map(documents,hispace,":")
documents <- tm_map(documents,hispace,"_")

writeLines(as.character(documents[[1]]))
##remove punctuations
documents <- tm_map(documents,removePunctuation)
##convert to lower case
documents <- tm_map(documents,content_transformer(tolower))
#keep checking the document line by line 
##let us remove numbers
documents <- tm_map(documents,removeNumbers)
##remove stopwords
documents <- tm_map(documents,removeWords,stopwords("english"))
#strip whitespace
documents <- tm_map(documents,stripWhitespace)
writeLines(as.character(documents[[1]]))
##most of the cleaning has been performed there are however some characters which remain undectable 
#text mining is an iterative process and therefore constant checking is necessary
##there are some words which require stemming. the library required for stemming is SnowballC
##stemming is performed for words which consist of unecessary prefix and suffix
documents<- tm_map(documents,stemDocument)
##however i wont be performing stemming because i need all the words in my document
##let us now create a document term matrix function
writeLines(as.character(documents[[1]]))
dtm <- DocumentTermMatrix(documents)
dtm # it has a sparsity of 39% which is good 
#let us inspect the dtm
##unfortunately dtm cannot be directly manipulated for the tidy sentiment analysis
##therefore, let us convert it to the tidy data format
tidy_dtm <- tidy(dtm)
#let us now check the tidy format of our documents
tidy_dtm
##now let us perform sentiment analysis
doc_sentiments <- tidy_dtm %>% 
  inner_join(get_sentiments("bing"),by=c(term='word'))
doc_sentiments

#now let us plot these 
library(ggplot2)
doc_sentiments %>% 
  count(sentiment,term,wt=count) %>% 
  ungroup %>% 
  filter(n>=2) %>% 
  mutate(n=ifelse(sentiment=="negative",-n,n)) %>% 
  mutate(term=reorder(term,n)) %>% 
  ggplot(aes(term,n,fill=sentiment))+
  geom_bar(stat='identity')+
  ylab("contribution to sentiment")+
  coord_flip()

#let us now start comparing the two documents and their sentiment analysis separately

documents_tf_idf <- doc_sentiments %>% 
  bind_tf_idf(term,document,count) %>% 
  arrange(desc(tf_idf))
documents_tf_idf


library(dplyr)
library(tidytext)
#just the count
ggplot(doc_sentiments,aes(sentiment,count, fill = document)) +
  geom_col(show.legend = FALSE) +
  geom_bar(stat='identity')+
  facet_wrap(~document, ncol = 10, scales = "free_x")
#with the tf_idf
ggplot(documents_tf_idf, aes(sentiment,tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  geom_bar(stat='identity')+
  facet_wrap(~document, ncol = 10, scales = "free_x")
#now let us try to extract the happy term from millenials 

millenials <- doc_sentiments %>% 
  filter(document=="millenials.txt")
millenials


millenials %>% 
  count(term,sort=TRUE)
View(millenials)

millenials %>% 
  filter(count>3) %>% 
  ggplot(aes(term,count,fill=sentiment))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

##similarly let us try to extract the analysis for baby boomers

baby_boomers<- doc_sentiments %>% 
  filter(document=="babyboomers.txt")
baby_boomers


baby_boomers %>% 
  count(term,sort=TRUE)
View(baby_boomers)

baby_boomers %>% 
  filter(count>3) %>% 
  ggplot(aes(term,count,fill=sentiment))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
