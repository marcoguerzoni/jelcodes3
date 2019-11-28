#a test to see how a new package is working

install.packages("text2vec")
library(stringr)
library(text2vec)

data("movie_review")
load("finalcorpus.Rdata")
id<-c(1:1652)
tokens = myCorp$content %>% 
  tolower %>% 
  word_tokenizer
it = itoken(tokens, ids = id, progressbar = FALSE)
v = create_vocabulary(it) #%>% 
  #prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")


lda_model = LDA$new(n_topics = 27, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)

lda_model$plot()

new_dtm = itoken(myCorp$content[1:50], tolower, word_tokenizer, ids = movie_review$id[4001:5000]) %>% 
  create_dtm(vectorizer, type = "dgTMatrix")
new_doc_topic_distr = lda_model$transform(new.dtm)