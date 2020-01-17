
install.packages("NLP")
install.packages("pdftools")
install.packages("tm")
install.packages("proxy")
install.packages("topicmodels")
install.packages("mime")
install.packages("stringr")
install.packages("tesseract")
install.packages("textclean")
install.packages("SnowballC")
install.packages("tidytext")
install.packages("purrr")
install.packages("udpipe")
install.packages("rlist")
###seet working directory to file location
setwd("C:/Users/despina/Google Drive/ricerca/myprojects/jelcodes3")
#very important

library(pdftools)
library(purrr)
library(NLP)
library(tm)
library(proxy) #require last version of R
library(topicmodels)
library(httpuv)
library(mime)
#library(servr)
library(stringr)
library(tesseract)
library(textclean)
source("ldavis_function.R")
source("removecommonterms.R")
library(remotes)
library(tidyr)
library(tidytext)
library(ggplot2)
library(rlist)
library(LDAvis)
#install.packages("tesseract", dependencies = TRUE)
#try it
#prova <- ocr("M19.pdf")
#change working directory
#setwd("C:/Users/despina/Google Drive/ricerca/myprojects/jelcodes")
#setwd("~/Marco Dropbox/marco guerzoni/TUTTI")
a <- list.files(pattern = "pdf$")
a <- c("19.pdf")
#save(a, file="filelist.Rdata")

#a<- a[1:500]

# b <- lapply(txt2, nchar)
# b<- unlist(b, use.names=TRUE)
# c <- 1:1652
# d <- as.data.frame(cbind(b,c))
# d <- cbind(d,c)
# row.names(d)<-c
# colnames(d)<- c("num", "id")
# d <- d[order(d$num),] 
# 
# file_list2[499]
# 
# nchar(txt[2])

#txt <- lapply(a, ocr)
text <- lapply(a, pdf_text)
txt <- text
#i create a corpus and a dcument ter line. I check which documents have zero entries
#they have zero sicne pdf_text did not work, therefore need ocr.
corp <- Corpus(VectorSource(txt))

dtm <- DocumentTermMatrix(corp,control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers= TRUE,stemming = TRUE ,stopwords = TRUE,minWordLength = 3))
rowTotals <- apply(dtm, 2, sum) #Find the sum of words in each Document
check<-which(rowTotals==0, arr.ind=TRUE)

#ocr only documents with problems
for (i in 1:1652){
  
  if(i %in% check==TRUE){ text[i]<-ocr(a[i])} 
  print(i)
}
#Sys.time()
#save.image()
#start cleaning
txt <- text
txt <- lapply(txt, gsub, patter="[\r\n]", replacement=" ")

#ocr<-c("A10.pdf","A10.pdf", "A29.pdf", "D40.pdf", "F95.pdf", "g19.pdf", "g53.pdf", "J85.pdf", "L105.pdf", "M4.pdf", "038.pdf", "P28.pdf", "P72.pdf", "P73.pdf", "P89.pdf", "P93.pdf", "q67.pdf", "q87.pdf", "r30.pdf")


#file_list2<-substring(file_list, 3)



#txt2<-txt
library(rlist)
list.save(text, file = "dataharvestingNEWbis.RData")
