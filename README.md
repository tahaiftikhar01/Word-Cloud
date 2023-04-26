# Word-Cloud
In this project i'll be running API on twitter for the Apple tweets posted in 2015 and make a word cloud from it.
install.packages ('tm')
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("scales")
install.packages("reshape2")
#NATURAL LANGUAGE PROCESSSING
#FOR EXAMPLE AUTO CORRECT IN GMAIL/NLP/EMAIL WRITING IN CHAT GPT, PERSONALIZED RECOMMENDSATOINS 
#NLP IS SPECIFICALLY USED TO PROCESE HUMAN EMOTIONS, ITS A PART OF TEXT MINING
#YOU HAVE TO TRAIN THE NLP FOR BETTER RESULTS


#does not cover processing and content analysis and then sentiment analysis
rm(list=ls())
#load the data
 apple<- read.csv(file.choose(), header=T)
 
 #we're only intersted in the first column
 
 #TM package
 library(tm)
#only pre processing of text mining(cleaning the data), the data in apple a is in html format
convert <- iconv(apple$text, to = "UTF-8-mac") #this function is used to convert the html into binary format, does not apply on survey, only for safety purposes
corpus <- Corpus(VectorSource(convert)) #help
inspect(corpus[1:5])
 

#content transformations
corpus <- tm_map(corpus, tolower)  #converting all the text into lower case
inspect (corpus[1:5]) #first 5 only
corpus <- tm_map(corpus, removePunctuation) #removing punatuation
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english')) #removing all the common words like i,u,etc from the data
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', "", x) #"" shows the replacement for the pattern
#we created a function to remove any alpha numerica text following the word'https'
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset)

#processsing (term document matrix)
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

tdm [1:10, 1:20]                   
                   
#pre-processing again
cleanset <- tm_map(cleanset, removeWords, c('appl', 'apple', 'optionsnipper', 'aapl',
                                            'dont','anything', 'head...', 'puts','might',
                                            'things',
                                            'est'))
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset)

#barplot to understand the frequencies
freq <- rowSums(tdm)
freq <- subset(freq, freq >= 50)  #only the terms greater than 25 will stay while the ones less than 25 will be removed

barplot(freq,
        las=2,
        col=rainbow(50))

#pre-processing again
cleanset <- tm_map(cleanset, gsub, pattern = 'stocks',
                   replacement = 'stock')
cleanset<- tm_map(cleanset, stripWhitespace)
inspect(cleanset)


#word clouds
library(wordcloud)
wc <- sort(rowSums(tdm),decreasing = T)
set.seed(222)
wordcloud(words = names(wc),
          freq = wc,
          max = 150, #biggest word, most freq
          min = 25, #min freq word
          random.order = F,
          colors = brewer.pal(8, 'Dark2'), #?brewer.pal is used to choose the colour palette
          scale = c(10,0.3),
          rot.per = 0.3) #30%

 #sentiment analysis
library (syuzhet)
library (lubridate)
library (ggplot2)
library (scales)
library(reshape?)
library (dplyr)
 
apple<- read.csv(file.choose(), header=T)
convert <- iconv(apple$text, to = "UTF-8-mac") 

#obtaining sentiment score
s <- get_nrc_sentiment(convert)  #nrc is brokendown into 8 differsant emotions for sentiment analysis, its  a huge dictionary
head(s)
convert [5]
get_nrc_sentiment("ugly")
get_nrc_sentiment("delay")

#barplot
barplot(colSums(s),
        las=2,
        col = rainbow(10),
        ylab= "Count",
        main = "Sentiment Sources of Apple Tweets with Post- EPS")
<img width="361" alt="Screenshot 2023-04-27 at 2 21 42 AM" src="https://user-images.githubusercontent.com/97010579/234705679-12e76286-eb01-471b-96f8-2c539cafd153.png">


