#set directory to file location

library(topicmodels)

library(tidytext)
library(tidyr)
load("ldamodel.Rdata")


#take the OLD model and create dataset of topics
ap_lda<-ap_lda1
ap_topics <- tidy(ap_lda, matrix = "beta")
topics_old <- ap_topics %>% spread(term, beta)

load("ldamodelnew.Rdata")
ap_lda<-ap_lda1
ap_topics <- tidy(ap_lda, matrix = "beta")
topics_new <- ap_topics %>% spread(term, beta)

varold<-colnames(topics_old)
varnew<-colnames(topics_new)

newvar <- setdiff(varnew,varold)
newvar1 <- setdiff(varold,varnew)
interse<-intersect(varnew,varold)

dfnewvar <- data.frame(matrix(ncol = 39, nrow = 27))
colnames(dfnewvar) <- newvar
dfnewvar[is.na(dfnewvar)]<- 0

dfnewvar1 <- data.frame(matrix(ncol = 26, nrow = 27))
colnames(dfnewvar1) <- newvar1
dfnewvar1[is.na(dfnewvar1)]<- 0

new <- cbind(topics_new, dfnewvar1)
new <- new[ , order(names(new))]

old <- cbind(topics_old, dfnewvar)
old <- old[ , order(names(old))]

total<- rbind(old, new)
totalsame <-  rbind(old, old)

totalt <- t(total)
totaltsame <- t(totalsame)


library(lsa)
a <-cosine(totalt)
b <-cosine(totaltsame)
c <-cosine(totalt)
similarity  <- a[1:27, 28:54]
similaritysame  <- b[1:27, 28:54]

library(lattice)
heatmap(similarity)

max <- apply(similarity, 1, which.max)
print(levelplot(similarity, xlab = NULL , ylab = NULL , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right",tick.number = 10)))


print(levelplot(t(similaritynew), xlab = "Old Classification" , ylab = "New Classification" , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right", tick.number = 10)))
print(levelplot(t(similaritynew2), xlab = "Old Classification" , ylab = "Old Classification" , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right", tick.number = 10)))



library(igraph)
graph1 <- graph_from_incidence_matrix(similarity, weighted= TRUE)  
graph1 <- graph_from_incidence_matrix(similaritysame, weighted= TRUE)

graph1 <- delete_edges(graph1, E(graph1)[weight<0.50])

numbertopics<-27

tt<-c(rep("TRUE",numbertopics), rep("FALSE", numbertopics))
col<-c(rep("blue",numbertopics), rep("red", 19))
shape<-c(rep("square",numbertopics), rep("circle", 19))
labels <- c(rep(1:27,2))


V(graph1)$type<-tt

plot(graph1, types=V(graph1)$type,vertex.label=labels, vertex.size=7, layout=layout_as_bipartite) 



documents <- tidy(ap_lda, matrix = "gamma")
documents_wide <- documents %>% spread(topic, gamma)
documents_wide <- documents_wide[order(as.integer(documents_wide$document)),] 
#28 is 27 (number of topic) plus one


plotpaper<-t(documents_wide[612,2:28])
names<-as.factor(c(1:27))



plot<- as.data.frame(cbind(plotpaper, names))
colnames(plot)<-c("value","Topics")  
fig <- ggplot(plot, aes(x=names, y=plotpaper)) + geom_bar(stat="identity")
fig <- fig + xlab("Topics")
fig <- fig + ylab("Probability")


documents_wide$max <- apply(documents_wide[2:28], 1, which.max)

ap_lda$beta


library(ggplot2)
library(dplyr)

top_terms <- ap_topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(10, beta) %>% # get the top 10 most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) # arrange words in descending informativeness



ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


data<-read.csv("dati.csv", sep=",")
load("filelist.Rdata")
data[,"idnum"]<-1:1663

library(car)
data$ID <- recode(data$ID,"'b6'='b06'")



data<- data[order(data$ID),]

#old.lvl<-levels(data$ID)
#data$ID<-factor(data$ID, levels=c(sort(old.lvl, decreasing=F)))

attach(data)
data <- data[ -which(MISSING=='1'),]
detach(data)
#data <- data[order(data$idnum),] 
data[,"class"]<-substr(data[,"Level1"], 1, 1)

FINALE <- cbind(data, documents_wide)
write.csv(FINALE, "datasetcompleto.csv") 


FINALE$ID[89]
FINALE$'1'[471]
documents_wide$`1`[1652]
a[89]

# M<-as.data.frame(cbind(data$ID,a))
# M[,3]<-data$ID
# length(levels(data$ID))
# 
# 
# data[,"idnum"]<-1:1652
# prova<- read.csv("list.csv", sep=",")
# 
# data2 <- data[ ! data$idnum %in% prova$count, ]
# data <-data2

theta <- as.matrix(posterior(ap_lda)$topics)
#ci aggiungo la classe
theta_mean_by <- by(theta, data[,"class"], colMeans) 
theta_mean <- do.call("rbind", theta_mean_by)


# theta_mean_ratios <- theta_mean
# 
# for (ii in 1:nrow(theta_mean)) {
#   for (jj in 1:ncol(theta_mean)) {
#     theta_mean_ratios[ii,jj] <-theta_mean[ii,jj] / sum(theta_mean[ii,-jj])
#   }
# }
library(lattice)
# topics_by_ratio <- apply(theta_mean_ratios, 1,function(x) sort(x, decreasing = TRUE, index.return = TRUE)$ix)
# topics_most_diagnostic <- topics_by_ratio[1,]
matrix <- t(theta_mean)
heatmap(matrix) #, Colv = NA, Rowv = NA, col = terrain.colors(256))


png(filename="topic2jel2.png")
print(levelplot(matrix, xlab = NULL , ylab = NULL , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right",tick.number = 10)))
dev.off()

#visualizing
write.csv(matrix, file="topic2jel.csv")

########most represented topic

prevalence<-as.data.frame(colSums(theta))
png(filename="Topicprevalence.png")
print(barplot(prevalence$'colSums(theta)', main="Topic prevalence", xlab="Topic", names.arg=1:27, ylim=c(0, 150)) )
dev.off()

theta_dist <- theta
for (ii in 1:nrow(theta)) {
  for (jj in 1:ncol(theta)) {
    theta_dist[ii,jj] <-theta[ii,jj] / sum(theta[,jj])
  }
}

prevalence2<-as.data.frame(colSums(theta_dist))
barplot(prevalence2$'colSums(theta_dist)', main="Topic prevalence", xlab="Topic", names.arg=1:27) 



thetasq<-theta_dist*theta_dist
HH<- colSums(thetasq)
HHnorm <- (HH-(1/1652))/(1-(1/1652))
png(filename="herfindahl.png")
print(barplot(HH, main="Herfindahl Index", xlab="Topic", names.arg=1:27, ylim=c(0,0.008 )) )
dev.off()

library(lsa)
a <- cosine(ap_lda.terms, y = NULL)

#top 40 terms in each topic
ap_lda.terms <- as.matrix(terms(ap_lda,50))
write.csv(ap_lda.terms,file=paste("LDAGibbs",50,"TopicsToTerms.csv"))

ap_lda.terms <- as.matrix(terms(ap_lda,1000))
a <- split(ap_topics, ap_topics$topic)
b<- a[[1]][[2]]
b<-sapply(a, '[[',3)
dim(b)
c <-  cosine(b, y = NULL)
heatmap(c)
d <- ifelse(c<0.07,NA,c)

class1<- rep("JEL", 19)
class2<- rep("TOPIC", 19) #number is numebr of topcis
class3<- c(class1, class2)

library(lattice)
colors_plot  <- rev(category_levels$color)

print(levelplot(c , xlab = NULL , ylab = NULL , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right",tick.number = 10)))
library(igraph)
graph1 <- graph_from_adjacency_matrix(c, mode = c("undirected"), weighted = TRUE, diag = FALSE, add.colnames = TRUE, add.rownames = TRUE)

graph1 <- delete_edges(graph1, E(graph1)[weight<0.25])


V(graph1)$size <-(prevalence$'colSums(theta)')/10
E(graph1)$width <- E(graph1)$weight*7
l <- layout_with_fr(graph1)
png(filename="Topicnetwork.png")
plot(graph1,  remove.multiple = T,edge.color="gray40", vertex.color="orange", layout=l, vertex.label.color="black", main="Topic Proximity Network (cutoff=0.25)")
dev.off()

net2 <- graph_from_incidence_matrix(matrix, weighted=TRUE)

numbertopics<-27
class4<-c(colnames(matrix), rownames(matrix))
s<-c(rep("Topic",numbertopics), rep("JEL", 19))
tt<-c(rep("TRUE",numbertopics), rep("FALSE", 19))
col<-c(rep("blue",numbertopics), rep("red", 19))
shape<-c(rep("square",numbertopics), rep("circle", 19))
V(net2)$cluster<-s
V(net2)$type<-tt

V(net2)$color <- col

V(net2)$shape <- shape

V(net2)$label <- ""



V(net2)$label.cex=.4

V(net2)$label.font=2



net3 <- delete_edges(net2, E(net2)[weight<0.10])
png(filename="topic2jel09.png")

plot(net3, types=V(net3)$type,  vertex.label=class4, vertex.label.color="white", label.cex=8 ,vertex.size=7, vertex.shape=shape, layout=layout_as_bipartite,  asp=0.45, margin=0, vertex.frame.color = "blue",                 # Node border color
     # One of "none", "circle", "square", "csquare", "rectangle" "crectangle", "vrectangle", "pie", "raster", or "sphere"
     # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     # Character vector used to label the nodes
     vertex.color = "red",
     # Font family of the label (e.g."Times", "Helvetica")
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="blue",                           # Edge color
     # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="dashed") 
dev.off()

png(filename="JELandTOPIC.png")
plot(net3,  vertex.label.color="white", label.cex=8 ,vertex.size=6, vertex.shape=shape,  asp=0.45, margin=-0.1, vertex.frame.color = "blue",                 # Node border color
     # One of "none", "circle", "square", "csquare", "rectangle" "crectangle", "vrectangle", "pie", "raster", or "sphere"
     # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     # Character vector used to label the nodes
     vertex.color = col,
     # Font family of the label (e.g."Times", "Helvetica")
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=0.8,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="blue",                           # Edge color
     # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="solid",
     edge.curved=0.2 ) 
dev.off()




plot(net3)

################new way to cmpute similarity
colSums(matrix)
rowSums(matrix)

matrix2<-matrix(,nrow=nrow(matrix), ncol=ncol(matrix)) 
rownames(matrix2) = rownames(matrix)
colnames(matrix2) = colnames(matrix)
for (i in 1:nrow(matrix)) {
  for (j in 1:ncol(matrix)) {
    matrix2[i,j]<-matrix[i,j]/mean(matrix[i,])
  }
}

png(filename="topic2jel.png")
print(levelplot(matrix2, xlab = NULL , ylab = NULL , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right",tick.number = 10)))
dev.off()


matrix.sort <- function(matrix) {
  
  if (nrow(matrix) != ncol(matrix)) stop("Not diagonal")
  if(is.null(rownames(matrix))) rownames(matrix) <- 1:nrow(matrix)
  
  row.max <- apply(matrix,1,which.max)
  if(all(table(row.max) != 1)) stop("Ties cannot be resolved")
  
  matrix[names(sort(row.max)),]
}
 

similaritynew<-matrix.sort(t(similarity))
similaritynew2<-matrix.sort(t(similaritysame))


FINALE2 <- FINALE[,30:58]

codici <- unique(FINALE2$class)

for (i in 1:length(codici)){
  
  final3<- FINALE2[which(FINALE2$class==codici[i]),3:29]
  final3 <- gather(final3, topic, value, 1:27, factor_key=TRUE)

   plot <- ggplot(final3, aes(x=topic, y=value)) +
    geom_boxplot() +
    geom_jitter(color="red", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14), 
      axis.title.x = element_text(size = 14, hjust=0.5),
      axis.title.y = element_text(size = 14, hjust=0.5)
      
    ) +
    ggtitle(paste('JEL ', codici[i])) + xlab('\nTopics') + ylab('Probability\n')
  
   png(filename=paste("Distribution Jel_", codici[i], ".png"), width = 680, height = 480)
   print(plot)
   dev.off()  
   print(i)
}




final3<- FINALE2[which(FINALE2$class=="A"),3:29]
final3 <- gather(final3, topic, value, 1:27, factor_key=TRUE)
final3 %>%
  ggplot( aes(x=topic, y=value)) +
  geom_boxplot() +
  geom_jitter(color="red", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(size = 14, hjust=0.5),
    axis.title.y = element_text(size = 14, hjust=0.5)
    
  ) +
  ggtitle("JEL A") + xlab('\nTopics') + ylab('Probability\n')


ylab("Probability")



plotbis<- as.data.frame(cbind(theta[1692,], as.factor(c(1:27))))
colnames(plotbis)<-c("value","Topics")  
fig <- ggplot(plotbis, aes(x=Topics, y=value)) + geom_bar(stat="identity")
fig <- fig + scale_x_discrete(name ="Topics", limits=c(1:27))
fig <- fig + ylab("Probability")
fig
