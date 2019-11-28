load(ldamodel.Rdata)
load("finalcorpus.Rdata")
myCorp<-myCorp[776:892]
dtm <- DocumentTermMatrix(myCorp)

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