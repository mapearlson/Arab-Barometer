###########################Arab Barometer Prelim#################################
###Subsetting
setwd("~/Documents/Delma/Arab Barometer")
d <- read.csv("arab3.csv")
dim(d)
str(d)
attach("d")
sum(d$q20114)
q20114
levels(q20114)
str(q20114)
detach("d")
table(d$country)
iraq <- d[d$country=="Iraq",]
pairs(~q1003+q2031+q2011,data=iraq)

###Principal Component Analysis
num.iraq <- data.matrix(iraq)
n.iraq <- as.data.frame(num.iraq)
var <- apply(n.iraq, 2, var)
var.0 <- subset(var, var==0)
out.iraq <- n.iraq[,apply(n.iraq, 2, var, na.rm=TRUE) != 0]  ###remove columns with 0 var
out.iraq <- out.iraq[-c(1,2,3,4,5,6,11,14,16,17,18,29,46)]
pca.iraq <- prcomp(out.iraq, scale = TRUE)###PCA

Cols <- function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow=c(1,2))
plot(pca.iraq$x[,1:2], col=Cols(names), pch=19,xlab="Z1",ylab="Z2")
plot(pca.iraq$x[,c(1,3)], col=cols(names), pch=19, xlab="Z1",ylab="Z3")
summary(pca.iraq)

pve=100*pca.iraq$sdev^2/sum(pca.iraq$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
       col =" blue ")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")

biplot(pca.iraq, scale=0)
pca.iraq$sdev
pca.var <- pca.iraq$sdev^2
pve <- pca.var/sum(pca.var)



###K-Means Clustering
###K-means clustering is a simple and elegant approach for partitioning a data set into 
###K distinct, non-overlapping clusters. To perform K-means clustering, we must first 
###specify the desired number of clusters K; then the K-means algorithm will assign each 
###observation to exactly one of the K clusters.
h.iraq <- out.iraq
hc.comp=hclust(dist(h.iraq), method="complete")
hc.average=hclust(dist(h.iraq), method="average")
hc.single=hclust(dist(h.iraq), method="single")

par(mfrow=c(1,3))
plot(hc.comp,main="Complete Linkage", xlab="", sub="",
       cex =.9)
plot(hc.average , main="Average Linkage", xlab="", sub="",
       cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub="",
       cex =.9)

####Above, but with the whole set.#######################################
d <- read.csv("arab3.csv")
dim(d)
str(d)
attach("d")
sum(d$q20114)
q20114
levels(q20114)
str(q20114)
detach("d")
table(d$country)
iraq <- d[d$country=="Iraq",]
pairs(~q1003+q2031+q2011,data=iraq)

###Principal Component Analysis
data <- data.matrix(d)
data <- as.data.frame(data)
var.d <- apply(data, 2, var)
var.0d <- subset(var.d, var==0)
out.d <- data[,apply(data, 2, var, na.rm=TRUE) != 0]  ###remove columns with 0 var
out.d <- out.d[-c(1:9,11,15,18,20,22,39,61)]
pca.d <- prcomp(out.d, scale = TRUE)###PCA

Cols <- function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow=c(1,2))
plot(pca.d$x[,1:2], col=Cols(names), pch=19,xlab="Z1",ylab="Z2")
plot(pca.d$x[,c(1,3)], col=cols(names), pch=19, xlab="Z1",ylab="Z3")
summary(pca.d)

pve.d=100*pca.d$sdev^2/sum(pca.d$sdev^2)
par(mfrow=c(1,2))
plot(pve.d, type="o", ylab="PVE", xlab="Principal Component",
     col =" blue ")
plot(cumsum(pve.d), type="o", ylab="Cumulative PVE", xlab="
     Principal Component ", col =" brown3 ")

biplot(pca.d, scale=0)
pca.iraq$sdev
pca.var <- pca.iraq$sdev^2
pve <- pca.var/sum(pca.var)



###K-Means Clustering
###K-means clustering is a simple and elegant approach for partitioning a data set into 
###K distinct, non-overlapping clusters. To perform K-means clustering, we must first 
###specify the desired number of clusters K; then the K-means algorithm will assign each 
###observation to exactly one of the K clusters.
hc.comp=hclust(dist(h.iraq), method="complete")
hc.average=hclust(dist(h.iraq), method="average")
hc.single=hclust(dist(h.iraq), method="single")

par(mfrow=c(1,3))
plot(hc.comp,main="Complete Linkage", xlab="", sub="",
     cex =.9)
plot(hc.average , main="Average Linkage", xlab="", sub="",
     cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub="",
     cex =.9)

##############Logit#######################################
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
install.packages("mlogit")
library(mlogit)

data2 <- mlogit.data(d, varying=NULL, choice="q20112", shape = "wide")
data3 <- mlogit.data(data, varying=NULL, choice="q20112", shape = "wide")

data <- data.matrix(d)
data <- as.data.frame(data)
out.d <- data[,apply(data, 2, var, na.rm=TRUE) != 0]  ###remove columns with 0 var
out.d <- out.d[-c(1:9,11,15,18,20,22,39,61)]

d$q20112<-relevel(d$q20112,ref="I absolutely do not trust it")
multi.fit <- multinom(q20112~q1003+q1004+q1007a+q1011a+q1012+q1014+q2003+q2004ir+ 
           q101+q102a+q106,data=d)

mlogit.fit <- mlogit(q20112~ 1 |q1003+q1004+q1007a+q1011a+q1012+q1014+q2003+q2004ir+ 
               q101+q102a+q106, data=data3 ,reflevel="")
summary(glm.fit)



