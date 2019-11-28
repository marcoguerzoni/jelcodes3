install.packages('gtools')

load(ldamodel.Rdata)

###read file





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

setwd("./dati")
stopA<-read.csv("stopA.csv", sep=";", fileEncoding="latin1")
stopA1<-read.csv("stopA1.csv", sep=";", fileEncoding="latin1")
stop<-read.csv("stopwordseng.csv", sep=";", fileEncoding="latin1")
stopEM<-read.csv("stopEM.csv", sep=";",fileEncoding="latin1")
stopF<-read.csv("stopF.csv", sep=";")
stopGH<-read.csv("stopGH.csv", sep=";")
stopI<-read.csv("stopI.csv", sep=";")
stopL<-read.csv("stopL.csv", sep=",")

setwd("./articoli")
#install.packages("tesseract", dependencies = TRUE)
#try it
#prova <- ocr("M19.pdf")
#change working directory
#setwd("C:/Users/despina/Google Drive/ricerca/myprojects/jelcodes")
#setwd("~/Marco Dropbox/marco guerzoni/TUTTI")
b <- list.files(pattern = "pdf$")

require('gtools')

b <- mixedsort(b)

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
text <- lapply(b, pdf_text)
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
#list.save(text, file = "dataharvesting.RData")


#txt<- list.load("dataharvesting.RData")



#1462 documenti. 
summary(txt)

###change directory two analysis 
###upload stopwords and words to erase stop words are collected inv arious files

stopA<-read.csv("stopA.csv", sep=";", fileEncoding="latin1")
stopA1<-read.csv("stopA1.csv", sep=";", fileEncoding="latin1")
stop<-read.csv("stopwordseng.csv", sep=";", fileEncoding="latin1")
stopEM<-read.csv("stopEM.csv", sep=";",fileEncoding="latin1")
stopF<-read.csv("stopF.csv", sep=";")
stopGH<-read.csv("stopGH.csv", sep=";")
stopI<-read.csv("stopI.csv", sep=";")
stopL<-read.csv("stopL.csv", sep=",")


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

####stop and stopA### word to remove

txt<-txt3
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt<-gsub("http[^[:space:]]*", "", txt)
txt<-gsub("www[^[:space:]]*", "", txt)
txt<-gsub('[\u2013:\u2016]', "", txt)
txt<-stringi::stri_trans_general(txt, "latin-ascii")
txt <- gsub("[^\x20-\x7E]", "", txt)

txt <- replace_non_ascii(txt, replacement = "", remove.nonconverted = TRUE)

txt <- gsub("-","", txt,ignore.case=T)
txt <- removePunctuation(txt,preserve_intra_word_dashes = FALSE)
txt <- removeNumbers(txt)
txt <- stripWhitespace(txt)
txt<- tolower(txt)


#
#create a corpus
corp <- Corpus(VectorSource(txt))


######CLEAN CORPUS
corp  <- tm_map(corp , removeWords, stopA)
corp  <- tm_map(corp , removeWords, stopA1)
corp  <- tm_map(corp , removeWords, stopEM)
corp  <- tm_map(corp , removeWords, stopF)
corp  <- tm_map(corp , removeWords, stopGH)
corp  <- tm_map(corp , removeWords, stopI)
corp  <- tm_map(corp , removeWords, stopL)
corp  <- tm_map(corp , removeWords, c("download", "cognetti"))


corp  <- tm_map(corp , stemDocument)
corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,removeWords,stopwords("en"))
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,removeNumbers)
corp <- tm_map(corp, content_transformer(gsub), pattern = "signiufbc",replacement = "signific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbrst",replacement = "first")
corp <- tm_map(corp, content_transformer(gsub), pattern = "diufber",replacement = "differ")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbrm",replacement = "firm")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbnd",replacement = "find")
corp <- tm_map(corp, content_transformer(gsub), pattern = "speciufbc",replacement = "specific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eufbect",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbn",replacement = "defin")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coeufbcient",replacement = "coefficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "efufbci",replacement = "effici")
corp <- tm_map(corp, content_transformer(gsub), pattern = "proufbt",replacement = "profit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbx",replacement = "fix")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbow",replacement = "flow")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coefufbci",replacement = "coeffici")
corp <- tm_map(corp, content_transformer(gsub), pattern = "reufbect",replacement = "reflect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eufbort",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbnanc",replacement = "financ")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbnit",replacement = "definit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "aufbect",replacement = "affect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "beneufbt",replacement = "benefit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbscal",replacement = "fiscal")
corp <- tm_map(corp, content_transformer(gsub), pattern = "oufber",replacement = "offer")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbgur",replacement = "figur")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbnal",replacement = "final")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbv",replacement = "fi")
corp <- tm_map(corp, content_transformer(gsub), pattern = "identiufb",replacement = "identifi")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbeld",replacement = "field")
corp <- tm_map(corp, content_transformer(gsub), pattern = "proufbl",replacement = "profit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "proufbtabl",replacement = "profitabl")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ofufbci",replacement = "offici")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbcit",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eect",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbcit",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eect",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "signic",replacement = "signific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "signic",replacement = "signific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "specic",replacement = "specific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "rms",replacement = "firms")
corp <- tm_map(corp, content_transformer(gsub), pattern = "benet",replacement = "benefit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dierent",replacement = "different")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coecient",replacement = "coefficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "rst",replacement = "first")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dierenc",replacement = "differenc")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coefcient",replacement = "coefficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "inuencn",replacement = "influen")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ding",replacement = "finding")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dierenti",replacement = "differenti")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eect",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "sucient",replacement = "sufficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "decit",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "rstorder",replacement = "firstorder")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dicult",replacement = "difficult")
corp <- tm_map(corp, content_transformer(gsub), pattern = "difcult",replacement = "difficult")
corp <- tm_map(corp, content_transformer(gsub), pattern = "gure",replacement = "figure")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eectiv",replacement = "effectiv")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ort",replacement = "effort")
corp <- tm_map(corp, content_transformer(gsub), pattern = "conrm",replacement = "confirm")
corp <- tm_map(corp, content_transformer(gsub), pattern = "nds",replacement = "funds")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eecienc",replacement = "efficienc")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ecient",replacement = "efficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "aect",replacement = "affect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "exibl",replacement = "flexibl")
corp <- tm_map(corp, content_transformer(gsub), pattern = "transpeffort",replacement = "transport")
corp <- tm_map(corp, content_transformer(gsub), pattern = "expeffort",replacement = "export")
corp <- tm_map(corp, content_transformer(gsub), pattern = "repeffort",replacement = "report")

corp  <- tm_map(corp , removeWords, c("com","pro","tion","robert","outcom","york","jame","john","abl","ture","articl","sion","richard","michael","william","chicago"))



#create a dtm and clean
dtm <- DocumentTermMatrix(corp,control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers= TRUE,stemming = TRUE ,stopwords = TRUE,minWordLength = 3))
dtm1<-removeSparseTerms(dtm, 0.98)
dtm1 <- removeCommonTerms(dtm1 ,0.8)
library(topicmodels)

#check if all documents are there
rowTotals <- apply(dtm1[1:1652,] , 1, sum) #Find the sum of words in each Document
check<-which(rowTotals==0, arr.ind=TRUE)

#convert dtm1 into a corpus
dtm2list <- apply(dtm1, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert to a Corpus
myCorp <- Corpus(VectorSource(dtm2list))


#reconvert Coprus to DTM
dtm <- DocumentTermMatrix(myCorp)


setwd('..')
setwd('..')
load("ldamodel.Rdata")
test.topics <- posterior(ap_lda,dtm)
test.topics1 <- apply(test.topics$topics, 1, which.max)

test.topics[2]
hist(test.topics)
A<- test.topics[2]


library(lattice)
levelplot(t(A$topics), col.regions=heat.colors(500), xlab="Topics", ylab="Articles", Title="Topic prediction AER October 2019" )
B <- t(A$topics)
B[B < 0.1] <- NA
levelplot(B, col.regions=heat.colors(500), xlab="Topics", ylab="Articles", Title="Topic prediction AER October 2019" )


levelplot( t(data[c(nrow(data):1) , ]),
           col.regions=heat.colors(100))

png(filename="topicPrediction.png")
print(levelplot(B, xlab = NULL , ylab = NULL , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right",tick.number = 10)))
dev.off()

myCorp$content[17]
dtm$i[17]

