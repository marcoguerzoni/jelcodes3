#data harvesting, create coprus and
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

library(remotes)
library(tidyr)
library(tidytext)
library(ggplot2)
library(rlist)
library(LDAvis)
library(rlist)

#set working directory to file location

source("ldavis_function.R")
source("removecommonterms.R")
txt<- list.load("dataharvesting.RData")

setwd("dati")

load("stopwords.Rdata")

#1462 documenti. 
summary(txt)

###change directory two analysis 
###upload stopwords and words to erase stop words are collected inv arious files


####stop and stopA### word to remove


txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt<-gsub("http[^[:space:]]*", "", txt)
txt<-gsub("www[^[:space:]]*", "", txt)
txt<-gsub('[\u2013:\u2016]', "", txt)
txt<-stringi::stri_trans_general(txt, "latin-ascii")
txt <- gsub("[^\x20-\x7E]", "", txt)
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
corp  <- tm_map(corp , removeWords, STOP)

corp  <- tm_map(corp , removeWords, c("download", "cognetti"))

#transform in dtm and do some check. uncomment if necessary
# dtm <- DocumentTermMatrix(corp)
# sum(dtm)
# dim(dtm)
# # 
# my_words <- c("download", "cognetti")
# # 
# dtm3 <- DocumentTermMatrix(corp1, control=list(dictionary = my_words))
# df1 <- data.frame(docs = dtm$dimnames$Docs, as.matrix(dtm), row.names = NULL)
# head(df1)
# df1[,2] <- as.numeric(df1[,2])
# sum(df1[,2])


corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,removeWords,stopwords("en"))
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,removeNumbers)
corp <- tm_map(corp, content_transformer(gsub), pattern = "signiufbc",replacement = "signific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bufbrst\\b",replacement = "first")
corp <- tm_map(corp, content_transformer(gsub), pattern = "diufber",replacement = "differ")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbrm",replacement = "firm")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bufbnd\\b",replacement = "find")
corp <- tm_map(corp, content_transformer(gsub), pattern = "speciufbc",replacement = "specific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eufbect",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbn",replacement = "defin")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coeufbcient",replacement = "coefficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "efufbci",replacement = "effici")
corp <- tm_map(corp, content_transformer(gsub), pattern = "proufbt",replacement = "profit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bufbx\\b",replacement = "fix")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bufbow\\b",replacement = "flow")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coefufbci",replacement = "coeffici")
corp <- tm_map(corp, content_transformer(gsub), pattern = "reufbect",replacement = "reflect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\beufbort\\b",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbnanc",replacement = "financ")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbnit",replacement = "definit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "aufbect",replacement = "affect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "beneufbt",replacement = "benefit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbscal",replacement = "fiscal")
corp <- tm_map(corp, content_transformer(gsub), pattern = "oufber",replacement = "offer")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbgur",replacement = "figur")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbnal",replacement = "final")
#corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbv",replacement = "fi")
corp <- tm_map(corp, content_transformer(gsub), pattern = "identiufb",replacement = "identifi")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ufbeld",replacement = "field")
corp <- tm_map(corp, content_transformer(gsub), pattern = "proufbl",replacement = "profit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "proufbtabl",replacement = "profitabl")
corp <- tm_map(corp, content_transformer(gsub), pattern = "ofufbci",replacement = "offici")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbcit",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\beect\\b",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "deufbcit",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\beect\\b",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "signic",replacement = "signific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "signic",replacement = "signific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "specic",replacement = "specific")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\brms\\b",replacement = "firms")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bbenet\\b",replacement = "benefit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dierent",replacement = "different")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coecient",replacement = "coefficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\brst\\b",replacement = "first")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dierenc",replacement = "differenc")
corp <- tm_map(corp, content_transformer(gsub), pattern = "coefcient",replacement = "coefficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "inuencn",replacement = "influen")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bding\\b",replacement = "finding")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dierenti",replacement = "differenti")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eect",replacement = "effect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bsucient\\b",replacement = "sufficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bdecit\\b",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "rstorder",replacement = "firstorder")
corp <- tm_map(corp, content_transformer(gsub), pattern = "dicult",replacement = "difficult")
corp <- tm_map(corp, content_transformer(gsub), pattern = "difcult",replacement = "difficult")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bgure\\b",replacement = "figure")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eectiv",replacement = "effectiv")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bort\\b",replacement = "effort")
corp <- tm_map(corp, content_transformer(gsub), pattern = "conrm",replacement = "confirm")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bnds\\b",replacement = "funds")
corp <- tm_map(corp, content_transformer(gsub), pattern = "eecienc",replacement = "efficienc")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\becient\\b",replacement = "efficient")
corp <- tm_map(corp, content_transformer(gsub), pattern = "aect",replacement = "affect")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bexibl\\b",replacement = "flexibl")

corp <- tm_map(corp, content_transformer(gsub), pattern = "speffort",replacement = "effort")
corp <- tm_map(corp, content_transformer(gsub), pattern = "peffortugal",replacement = "portugal")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bhealthca\\b",replacement = "healthcare")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\begion\\b",replacement = "region")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bdolla\\b",replacement = "dollar")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\beputational\\b",replacement = "repuatational")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bespondent\\b",replacement = "respondent")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bespondents\\b",replacement = "respondents")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bnancial\\b",replacement = "financial")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bnanc\\b",replacement = "finance")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bnanci\\b",replacement = "finance")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bnding\\b",replacement = "finding")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bdefi\\b",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bdefici\\b",replacement = "deficit")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bticular\\b",replacement = "particular")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bditur\\b",replacement = "expenditur")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\brregion\\b",replacement = "region")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\brrrespond\\b",replacement = "respond")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\beort\\b",replacement = "effort")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bvestement\\b",replacement = "investement")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\bacteristic\\b",replacement = "characteristic")
corp <- tm_map(corp, content_transformer(gsub), pattern = "\\btistic\\b",replacement = "characteristic")



corp  <- tm_map(corp , stemDocument)
corp  <- tm_map(corp , removeWords, c("com","pro","tion","robert","outcom","york","jame","john","abl","ture","articl","sion","richard","michael","william","chicago","dene","deni","das","daytoday","supportjstororg","olog","ici", "tice", "tical","tifi", "tic", "palgrave", "mcgrawhil", "macmillan", "iti", "hor", "doi", "fli", "alfr", "dordrecht", "addisonwesley","ogi","springerverlag", "download"))



#create a dtm and clean
dtm <- DocumentTermMatrix(corp,control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers= TRUE,stemming = TRUE ,stopwords = TRUE,minWordLength = 3))
dtm1<-removeSparseTerms(dtm, 0.97)
dtm1 <- removeCommonTerms(dtm1 ,0.8)
library(topicmodels)

#check if all documents are there
# rowTotals <- apply(dtm1[1:1652,] , 1, sum) #Find the sum of words in each Document
# check<-which(rowTotals==0, arr.ind=TRUE)

#convert dtm1 into a corpus
dtm2list <- apply(dtm1, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert to a Corpus
myCorp <- Corpus(VectorSource(dtm2list))


#reconvert Coprus to DTM
dtm <- DocumentTermMatrix(myCorp)

#DTM and myCorp are the final data to use.

setwd("..")
save(dtm, file="finaldtm.Rdata")
load("finaldtm.Rdata")
save(myCorp, file = "finalcorpus.Rdata")
load("finalcorpus.Rdata")
####some statistic
wordcount <- colSums(as.matrix(dtm))
topten <- head(sort(wordcount, decreasing=TRUE), 10)
many <- head(sort(wordcount, decreasing=TRUE), 10000)

library(reshape2)


#plot topten
dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)


png(filename="topten.png")
plot(fig)
dev.off()


dfplot <- as.data.frame(melt(many))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")+ 
  theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
print(fig)

install.packages("wordcloud") # word-cloud generator
library(wordcloud)
set.seed(1234)
wordcloud<- wordcloud(words = names(wordcount), freq = wordcount, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

png(filename="wordcloud.png")
plot(wordcloud)
dev.off()


#22 311
# 500+
#1000
#dtm.new   <- dtm[rowTotals> 0, ] 
ap_lda1 <- LDA(dtm, 27, method = "Gibbs",control = list(iter = 100, seed = 33))

save(ap_lda1, file = "ldamodel.Rdata")
















serVis(topicmodels2LDAvis(ap_lda1),  out.dir = 'vis')

serVis(topicmodels2LDAvis(ap_lda1))


#further check
# coffee_m <- as.matrix(dtm1)
# 
# # Calculate the rowSums: term_frequency
# term_frequency <- colSums(coffee_m)
# 
# # Sort term_frequency in descending order
# term_frequency <- sort(term_frequency, decreasing = F)
# 
# # View the top 10 most common words
# term_frequency[1:100]
# 
# #order dtm, check for some lines if the ordered frequency of terms correspond to articel
# matrix <- as.matrix(dtm)
# matrix <-matrix[,order(matrix[1652,], decreasing = T)]
# matrix[1652,1:10]
# #return the pdf document
# a[1652]
