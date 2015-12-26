#install.packages('tm')
#install.packages('wordcloud')

library(tm)
library(wordcloud)

setwd("/home/campbwa/Dropbox/wordclouds/thomas/text")


b=Corpus(DirSource())
#sometimes you have to view hidden files and remove the .Rhistory and backup files!



#list.files()
#b = scan(file ='lyrics.txt', what = "character")

b <- tm_map(b, tolower) #Changes case to lower case

b<- tm_map(b, stripWhitespace) #Strips White Space

b <- tm_map(b, removePunctuation) #Removes Punctuation
b <- tm_map(b, removeWords, stopwords("english")) 
b <- tm_map(b, removeWords, c("and","the", "with", "was", "200", "2008", "2010", "2011", 
                              "647", "222", "223", "115", "1994", "2007", "2012", "2013", "for",
                              "are", "have", "this", "has", "2009")) 

tdm <- TermDocumentMatrix(b)

m1 <- as.matrix(tdm)

v1<- sort(rowSums(m1),decreasing=TRUE)

v1 = v1[1:50]

d1<- data.frame(word = names(v1),freq=v1)

#wordcloud(d1$word,d1$freq)




#some colors
  pal <- brewer.pal(9,"BuGn")
  pal <- pal[-(1:4)]
  wordcloud(d1$word,d1$freq,c(8,.3),2,,FALSE,,.15,pal)
  
  
  pal <- brewer.pal(6,"Dark2")
  pal <- pal[-(1)]
  wordcloud(d1$word,d1$freq,c(8,.3),2,,TRUE,,.15,pal)
  
  #random colors
  wordcloud(d1$word,d1$freq,c(5,.2),2,,TRUE,TRUE,.15,pal)


pdf("../thomas wordcloud1.pdf")
wordcloud(d1$word,d1$freq,c(5,.2),2,,TRUE,TRUE,.15,pal)
dev.off()

png("../thomas wordcloud.png")
wordcloud(d1$word,d1$freq,c(8,.3),2,,TRUE,TRUE,.15,pal)
dev.off()
