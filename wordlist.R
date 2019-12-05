# create list words to remove

library(purrr)
library(NLP)
library(tm)
library(proxy) #require last version of R

library(httpuv)
library(mime)
#library(servr)
library(stringr)
library(tesseract)
library(textclean)
source("ldavis_function.R")
source("removecommonterms.R")
library(remotes)

library(rlist)


setwd("dati")
stopA<-read.csv("stopA.csv", sep=";", fileEncoding="latin1")
stopA1<-read.csv("stopA1.csv", sep=";", fileEncoding="latin1")
stop<-read.csv("stopwordseng.csv", sep=";", fileEncoding="latin1")
stopEM<-read.csv("stopEM.csv", sep=";",fileEncoding="latin1")
stopF<-read.csv("stopF.csv", sep=";")
stopGH<-read.csv("stopGH.csv", sep=";")
stopI<-read.csv("stopI.csv", sep=";")
stopL<-read.csv("stopL.csv", sep=",")
stopdurio<-read.csv("stopdurio.csv", sep=",")

#clean stopwordsfile

stop <- toString(stop[,1])
#stop <- removePunctuation(stop ,preserve_intra_word_dashes = TRUE)
stop <- strsplit(stop, "[[:space:]]+")
stop <- unlist(stop)
stop <- removePunctuation(stop ,preserve_intra_word_dashes = TRUE)
stop <- removeNumbers(stop)
stop<- tolower(stop)

stopA <- apply(stopA, 2, toString)
stopA<- stripWhitespace(stopA)
#stopA <- removePunctuation(stopA ,preserve_intra_word_dashes = TRUE)
stopA<- stripWhitespace(stopA)
stopA <- strsplit(stopA, "[[:space:]]+")
stopA <- unlist(stopA)
stopA <- removePunctuation(stopA ,preserve_intra_word_dashes = TRUE)
stopA <- removeNumbers(stopA)
stopA<- tolower(stopA)


stopA1 <- apply(stopA1, 2, toString)
stopA1<- stripWhitespace(stopA1)
#stopA1 <- removePunctuation(stopA1 ,preserve_intra_word_dashes = TRUE)
stopA1 <- strsplit(stopA1, "[[:space:]]+")
stopA1 <- unlist(stopA1)
stopA1 <- removePunctuation(stopA1 ,preserve_intra_word_dashes = TRUE)
stopA1 <- removeNumbers(stopA1)
stopA1<- tolower(stopA1)


stopEM <- apply(stopEM, 2, toString)
stopEM<- stripWhitespace(stopEM)
#stopEM <- removePunctuation(stopEM ,preserve_intra_word_dashes = TRUE)
stopEM <- strsplit(stopEM, "[[:space:]]+")
stopEM<- unlist(stopEM)
stopEM <- removePunctuation(stopEM ,preserve_intra_word_dashes = TRUE)
stopEM <- removeNumbers(stopEM)
stopEM<- tolower(stopEM)


stopF <- apply(stopF, 2, toString)
stopF<- stripWhitespace(stopF)
#stopF <- removePunctuation(stopF ,preserve_intra_word_dashes = TRUE)
stopF<- strsplit(stopF, "[[:space:]]+")
stopF<- unlist(stopF)
stopF <- removePunctuation(stopF ,preserve_intra_word_dashes = TRUE)
stopF<- removeNumbers(stopF)
stopF<- tolower(stopF)

stopGH <- apply(stopGH, 2, toString)
stopGH<- stripWhitespace(stopGH)
#stopGH <- removePunctuation(stopGH ,preserve_intra_word_dashes = TRUE)
stopGH<- strsplit(stopGH, "[[:space:]]+")
stopGH<- unlist(stopGH)
stopGH <- removePunctuation(stopGH ,preserve_intra_word_dashes = TRUE)
stopGH<- removeNumbers(stopGH)
stopGH<- tolower(stopGH)


stopI <- apply(stopI, 2, toString)
stopI<- stripWhitespace(stopI)
#stopI <- removePunctuation(stopI ,preserve_intra_word_dashes = TRUE)
stopI<- strsplit(stopI, "[[:space:]]+")
stopI<- unlist(stopI)
stopI <- removePunctuation(stopI ,preserve_intra_word_dashes = TRUE)
stopI<- removeNumbers(stopI)
stopI<- tolower(stopI)


stopL <- apply(stopL, 2, toString)
stopL<- stripWhitespace(stopL)
stopL <- removePunctuation(stopL ,preserve_intra_word_dashes = TRUE)
stopL<- strsplit(stopL, "[[:space:]]+")
stopL<- unlist(stopL)
stopL <- removePunctuation(stopL ,preserve_intra_word_dashes = TRUE)
stopL<- removeNumbers(stopL)
stopL<- tolower(stopL)

stopdurio <- apply(stopdurio, 2, toString)
stopdurio<- stripWhitespace(stopdurio)
stopdurio<- strsplit(stopdurio, "[[:space:]]+")
stopdurio<- unlist(stopdurio)


STOP <-c(stopL, stopI, stopGH, stopF, stopEM, stopA1, stopA, stopdurio)
STOP <- removePunctuation(STOP ,preserve_intra_word_dashes = TRUE)
STOP<- removeNumbers(STOP)
STOP <- as.vector(STOP)
STOP <- STOP[which(STOP!="na")]
STOP <- STOP[which(STOP!="")]
STOP <- STOP[which(STOP!="")]

#adjusting for some words I wnat to retain
retain <- c("eputational", "espondent", "espondents", "healthca", "coeffiecient", "school", "wellbeing", "option", "dolla", "choice", "egion")
STOP <- STOP[!STOP %in% retain]

save(STOP, file="stopwords.Rdata")
