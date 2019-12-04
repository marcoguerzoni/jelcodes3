#data harvesting, create coprus and

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

setwd("C:/Users/despina/Google Drive/ricerca/myprojects/jelcodes")
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
list.save(text, file = "dataharvesting.RData")


txt<- list.load("dataharvesting.RData")



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

txt <- replace_non_ascii(txt, replacement = "", remove.nonconverted = TRUE)
txt <- gsub("-","", txt,ignore.case=T)
txt <- removePunctuation(txt,preserve_intra_word_dashes = FALSE)
txt <- removeNumbers(txt)
txt <- stripWhitespace(txt)
txt<- tolower(txt)

load("stopwords.Rdata")
#
#create a corpus
corp <- Corpus(VectorSource(txt))


######CLEAN CORPUS
corp  <- tm_map(corp , removeWords, STOP)

corp  <- tm_map(corp , removeWords, c("download", "cognetti"))

dtm <- DocumentTermMatrix(corp)
sum(dtm)
dim(dtm)
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
corp <- tm_map(corp, content_transformer(gsub), pattern = "speffort",replacement = "effort")
corp <- tm_map(corp, content_transformer(gsub), pattern = "peffortugal",replacement = "portugal")

corp  <- tm_map(corp , removeWords, c("com","pro","tion","robert","outcom","york","jame","john","abl","ture","articl","sion","richard","michael","william","chicago"))



#create a dtm and clean
dtm <- DocumentTermMatrix(corp,control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers= TRUE,stemming = TRUE ,stopwords = TRUE,minWordLength = 3))
dtm1<-removeSparseTerms(dtm, 0.97)
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

#DTM and myCorp are the final data to use.


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















#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

# topicmodels_json_ldavis <- function(fitted, doc_term){
#   require(LDAvis)
#   require(slam)
#   
#   # Find required quantities
#   phi <- as.matrix(posterior(fitted)$terms)
#   theta <- as.matrix(posterior(fitted)$topics)
#   vocab <- colnames(phi)
#   term_freq <- slam::col_sums(doc_term)
#   
#   # Convert to json
#   json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
#                                  vocab = vocab,
#                                  doc.length = as.vector(table(doc_term$i)),
#                                  term.frequency = term_freq)
#   
#   return(json_lda)
# }



#ldavis, the function topcimodel2LDAvis is source() at the begining
serVis(topicmodels2LDAvis(ap_lda1),  out.dir = 'vis')

serVis(topicmodels2LDAvis(ap_lda1))
#library(tidytext)

#ap_topics <- tidy(ap_lda, matrix = "beta")

#library(ggplot2)
#library(dplyr)

# ap_top_terms <- ap_topics %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# ap_top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()
# 
# ####
# file_list <- list.files(".", full.names = TRUE, pattern = '.pdf$')
# 
# s_pdf_text <- safely(pdf_text) # helps catch errors
# 
# walk(file_list, ~{                                     # iterate over the files
#   
#   res <- s_pdf_text(.x)                                # try to read it in
#   if (!is.null(res$result)) {                          # if successful
#     
#     message(sprintf("Processing [%s]", .x))
#     
#     txt_file <- sprintf("%stxt", sub("pdf$", "", .x))  # make a new filename
#     
#     unlist(res$result) %>%                             # cld be > 1 pg (which makes a list)
#       tolower() %>%                                    
#       paste0(collapse="\n") %>%                        # make one big text block with line breaks
#       cat(file=txt_file)                               # write it out
#     
#   } else {                                             # if not successful
#     message(sprintf("Failure converting [%s]", .x))    # show a message
#   }
#   
# })
# a<-a[!a=="g32.pdf"]


# > file.copy(from="stopA1.csv", to="~/", 
#             +           overwrite = TRUE, recursive = FALSE, 
#             +           copy.mode = TRUE)



coffee_m <- as.matrix(dtm1)

# Calculate the rowSums: term_frequency
term_frequency <- colSums(coffee_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = F)

# View the top 10 most common words
term_frequency[1:100]

#order dtm, check for some lines if the ordered frequency of terms correspond to articel
matrix <- as.matrix(dtm)
matrix <-matrix[,order(matrix[1652,], decreasing = T)]
matrix[1652,1:10]
#return the pdf document
a[1652]
