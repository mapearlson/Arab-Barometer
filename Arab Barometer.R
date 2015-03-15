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

##############Multinomial Logit#######################################
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
install.packages("mlogit")
library(mlogit)

###reshape from wide to long format
mldata <- mlogit.data(d, varying=NULL, choice="q20112", shape = "wide")
mldata.int <- data.matrix(mldata)
mldata.int <- as.data.frame(mldata.int)
mlogit <- mlogit(q20112 ~ 0 | q1003+q1004+q1007a+q1011a+q1012+q1014+q2003+q2004ir+ 
                   q101+q102a+q106,data=mldata, reflevel="I trust it to a great extent")
mlogit <- mlogit(q20112 ~ 0 | q1003+q1004+q1007a+q1011a+q2003+q2004ir+ 
                   q101,data=mldata)
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

############################Binary Logit#####################################
require(plyr)
require(leaps)
###Remove Missings
d2 <- sapply(d, as.character, d[,1:296])
d2[d2==""] <- NA
d2[d2=="Don't Know"] <- NA
d2[d2=="Refuse"] <- NA
d2[d2=="Missing"] <- NA
d3 <- as.data.frame(d2)
d4 <- data.matrix(d3)
d4 <- as.data.frame(d4)

###recode outcome var
d3$islam.p <- revalue(d3$q20112, c("I absolutely do not trust it"="0",
                                   "I trust it to a limited extent"="0",
                                   "I trust it to a medium extent"="1",
                                  "I trust it to a great extent"= "1"))
d3$islam.p2 <- revalue(d3$q20112, c("I absolutely do not trust it"="No Support",
                                   "I trust it to a limited extent"="No Support",
                                   "I trust it to a medium extent"="Support",
                                   "I trust it to a great extent"= "Support"))

d4$islam.p[d4$q20112==4] <- 1
d4$islam.p[d4$q20112==2] <- 1
d4$islam.p[d4$q20112==1] <- 0
d4$islam.p[d4$q20112==3] <- 0

###subset selection
install.packages("fields")
library(fields)
t(stats(d4))
d6<-d3
d5 <- d4
###Factor Egypt
d2.egypt <- subset(d6, country=="Egypt") 
d2.egypt<-d2.egypt[-c(1:8,30,297)]
d2.egypt <- d2.egypt[!colSums(is.na(d2.egypt)) >= 35]
d2.egypt<-na.omit(d2.egypt)

e2.regfit.fwd <- regsubsets(islam.p2~., d2.egypt,nvmax=5, method = "forward")
e2.sum.fwd <- summary(e2.regfit.fwd)
e2.regfit.bwd <- regsubsets(islam.p2~.,d2.egypt,nvmax=5, method = "backward")
e2.sum.bwk <- summary(e2.regfit.bwd)

###Only Egypt Forward and Backward Selection
d.egypt <- subset(d5, country==2) 
d.egypt<-d.egypt[-c(1:9,30,104,297)]
d.egypt <- d.egypt[!colSums(is.na(d.egypt)) >= 35]
d.egypt<-na.omit(d.egypt)

e.regfit.fwd <- regsubsets(islam.p~., d.egypt,nvmax=5, method = "forward")
e.sum.fwd <- summary(e.regfit.fwd)
e.regfit.bwd <- regsubsets(islam.p~.,d.egypt,nvmax=5, method = "backward")
e.sum.bwk <- summary(e.regfit.bwd)

par(mfrow=c(2,2))
plot(e.sum.fwd$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
plot(e.sum.fwd$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")

which.max(e.sum.fwd$adjr2)
points(6,e.sum.fwd$adjr2[6], col="red",cex=2,pch=20)

plot(e.sum.fwd$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
which.min(e.sum.fwd$cp )
points(6,e.sum.fwd$cp [6],col="red",cex=2,pch=20)
which.min(e.sum.fwd$bic )
plot(e.sum.fwd$bic ,xlab="Number of Variables ",ylab="BIC",
       type='l')
points(6,e.sum.fwd$bic [6],col="red",cex=2,pch=20)

plot(e.regfit.fwd,scale="r2")
plot(e.regfit.fwd,scale="adjr2")
plot(e.regfit.fwd,scale="Cp")
plot(e.regfit.fwd,scale="bic")

coef(e.regfit.fwd ,6)
coef(e.regfit.bwd ,6)

e.log.fit <- glm(islam.p~q105a+q2011+q20113+q2042+q409+q501b, 
                 data=d.egypt,family=binomial)
summary(e.log.fit)
efac.log.fit <- glm(islam.p~factor(q105a)+factor(q2011)+factor(q20113)+
                      factor(q2042)+factor(q409)+factor(q501b), data=d.egypt,family=binomial)
summary(efac.log.fit)

exp(cbind(OR = coef(e.log.fit), confint(e.log.fit)))

###Pred Probabilities
pred.frame <- with(d.egypt, data.frame(q105a = mean(q105a), q2011 = mean(q2011), 
                                     q20113 = mean(q20113), q2042 = (1:4), 
                                     q409= mean(q409), q501b = mean(q501b)))
fac.pred.frame <- with(d.egypt, data.frame(q105a = mean(q105a), q2011 = mean(q2011), 
                                       q20113 = mean(q20113), q2042 = factor(1:4), 
                                       q409= mean(q409), q501b = mean(q501b)))
pred.frame
fac.pred.frame

pred.prob <- predict(e.log.fit , newdata = pred.frame, type = "response")
fac.pred.prob <- predict(e.log.fit , newdata = pred.frame, type = "response")

###Sudan
d.sudan <- subset(d5, country==10)
d.sudan<-d.sudan[-c(1:9,30,104,297)]
d.sudan <- d.sudan[!colSums(is.na(d.sudan)) > 45]
d.sudan<-na.omit(d.sudan)

s.regfit.fwd <- regsubsets(islam.p~.,d.sudan,nvmax=5, method = "forward")
s.sum.fwd <- summary(regfit.fwd)
s.regfit.bwd <- regsubsets(islam.p~.,d.sudan,nvmax=5, method = "backward")
s.sum.bwk <- summary(regfit.bwd)

par(mfrow=c(2,2))
plot(s.sum.fwd$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(s.sum.fwd$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")

which.max(s.sum.fwd$adjr2)
points(6,s.sum.fwd$adjr2[6], col="red",cex=2,pch=20)

plot(s.sum.fwd$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
which.min(s.sum.fwd$cp )
points(6,s.sum.fwd$cp [6],col="red",cex=2,pch=20)
which.min(s.sum.fwd$bic )
plot(s.sum.fwd$bic ,xlab="Number of Variables ",ylab="BIC",
     type='l')
points(6,s.sum.fwd$bic [6],col="red",cex=2,pch=20)

plot(s.regfit.fwd,scale="r2")
plot(s.regfit.fwd,scale="adjr2")
plot(s.regfit.fwd,scale="Cp")
plot(s.regfit.fwd,scale="bic")

coef(s.regfit.fwd ,6)
coef(s.regfit.bwd ,6)

s.log.fit <- glm(islam.p~ q105a+q2013+q2062+q213+q6062+q6106, data=d.sudan,family=binomial)
summary(s.log.fit)

exp(cbind(OR = coef(s.log.fit), confint(s.log.fit)))

###Pred Probabilities
s.pred.frame <- with(d.sudan, data.frame(q105a = mean(q105a), q2013 = mean(q2013), 
                                       q2062 = mean(q2062), q213 = (1:4), 
                                       q6062= mean(q6062), q6106 = mean(q6106)))

s.pred.frame

pred.prob <- predict(e.log.fit , newdata = pred.frame, type = "response")
fac.pred.prob <- predict(e.log.fit , newdata = pred.frame, type = "response")

####Yemen
d.yemen <- subset(d5, country==12) 
d.yemen<-d.yemen[-c(1:9,30,104,297)]
d.yemen <- d.yemen[!colSums(is.na(d.yemen)) > 40]
d.yemen<-na.omit(d.yemen)

y.regfit.fwd <- regsubsets(islam.p~.,d.yemen,nvmax=5, method = "forward")
y.sum.fwd <- summary(regfit.fwd)
y.regfit.bwd <- regsubsets(islam.p~.,d.yemen,nvmax=5, method = "backward")
y.sum.bwk <- summary(y.regfit.bwd)

par(mfrow=c(2,2))
plot(y.sum.fwd$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(y.sum.fwd$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")

which.max(y.sum.fwd$adjr2)
points(6,y.sum.fwd$adjr2[6], col="red",cex=2,pch=20)

plot(y.sum.fwd$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
which.min(y.sum.fwd$cp )
points(6,y.sum.fwd$cp [6],col="red",cex=2,pch=20)
which.min(y.sum.fwd$bic )
plot(y.sum.fwd$bic ,xlab="Number of Variables ",ylab="BIC",
     type='l')
points(6,y.sum.fwd$bic [6],col="red",cex=2,pch=20)

plot(y.regfit.fwd,scale="r2")
plot(y.regfit.fwd,scale="adjr2")
plot(y.regfit.fwd,scale="Cp")
plot(y.regfit.fwd,scale="bic")

coef(y.regfit.fwd ,6)
coef(y.regfit.bwd ,6)

log.fit <- glm(islam.p~  q2011       q5012       q5211       q6106       q7007      q812a3 , data=d.egypt,family=binomial)
summary(log.fit)

all.d5 <- na.omit(d5)
all.d5 <- all.d5[,-c(30)]
regfit.fwd <- regsubsets(islam.p~.,all.d5,nvmax=10, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(islam.p~.,all.d5,nvmax=10, method = "backward")
summary(regfit.bwd)
log.fit <- glm(islam.p~q104 + q105a , data=d3,family=binomial)
summary(log.fit)

###Tunisia
d.egypt <- subset(d5, country==2) 
d.egypt<-d.egypt[-c(1:8,30,104,297)]
d.egypt <- d.egypt[!colSums(is.na(d.egypt)) > 100]
d.egypt<-na.omit(d.egypt)

regfit.fwd <- regsubsets(islam.p~.,d.egypt,nvmax=5, method = "forward")
sum.fwd <- summary(regfit.fwd)
regfit.bwd <- regsubsets(islam.p~.,d.egypt,nvmax=5, method = "backward")
sum.bwk <- summary(regfit.bwd)

par(mfrow=c(2,2))
plot(sum.fwd$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(sum.fwd$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")

which.max(sum.fwd$adjr2)
points(6,sum.fwd$adjr2[6], col="red",cex=2,pch=20)

plot(sum.fwd$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
which.min(sum.fwd$cp )
points(6,sum.fwd$cp [6],col="red",cex=2,pch=20)
which.min(sum.fwd$bic )
plot(sum.fwd$bic ,xlab="Number of Variables ",ylab="BIC",
     type='l')
points(6,sum.fwd$bic [6],col="red",cex=2,pch=20)

plot(regfit.fwd,scale="r2")
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="bic")

coef(regfit.fwd ,6)
coef(regfit.bwd ,6)

log.fit <- glm(islam.p~q2013+q202+q5021+q6052, data=d.egypt,family=binomial)
summary(log.fit)

all.d5 <- na.omit(d5)
all.d5 <- all.d5[,-c(30)]
regfit.fwd <- regsubsets(islam.p~.,all.d5,nvmax=10, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(islam.p~.,all.d5,nvmax=10, method = "backward")
summary(regfit.bwd)
log.fit <- glm(islam.p~q104 + q105a , data=d3,family=binomial)
summary(log.fit)

###Morocco
d.egypt <- subset(d5, country==2) 
d.egypt<-d.egypt[-c(1:8,30,297)]
d.egypt <- d.egypt[!colSums(is.na(d.egypt)) > 100]
d.egypt<-na.omit(d.egypt)

regfit.fwd <- regsubsets(islam.p~.,d.egypt,nvmax=5, method = "forward")
sum.fwd <- summary(regfit.fwd)
regfit.bwd <- regsubsets(islam.p~.,d.egypt,nvmax=5, method = "backward")
sum.bwk <- summary(regfit.bwd)

par(mfrow=c(2,2))
plot(sum.fwd$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(sum.fwd$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")

which.max(sum.fwd$adjr2)
points(6,sum.fwd$adjr2[6], col="red",cex=2,pch=20)

plot(sum.fwd$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
which.min(sum.fwd$cp )
points(6,sum.fwd$cp [6],col="red",cex=2,pch=20)
which.min(sum.fwd$bic )
plot(sum.fwd$bic ,xlab="Number of Variables ",ylab="BIC",
     type='l')
points(6,sum.fwd$bic [6],col="red",cex=2,pch=20)

plot(regfit.fwd,scale="r2")
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="bic")

coef(regfit.fwd ,6)
coef(regfit.bwd ,6)

log.fit <- glm(islam.p~q2013+q202+q5021+q6052, data=d.egypt,family=binomial)
summary(log.fit)

all.d5 <- na.omit(d5)
all.d5 <- all.d5[,-c(30)]
regfit.fwd <- regsubsets(islam.p~.,all.d5,nvmax=10, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(islam.p~.,all.d5,nvmax=10, method = "backward")
summary(regfit.bwd)
log.fit <- glm(islam.p~q104 + q105a , data=d3,family=binomial)
summary(log.fit)








