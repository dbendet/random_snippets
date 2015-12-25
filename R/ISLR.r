# regression
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

lm.fit=lm(medv~lstat, data=Boston)

summary(lm.fit)

names(lm.fit)

confint (lm.fit)

predict(lm.fit, data.frame(lstat=(c(5,10 ,15))), interval="confidence")

plot(lstat, medv)
abline(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


lm.fit=lm(medv~lstat+age, data=Boston )
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

?summary.lm

summary(lm.fit)$r.sq
summary(lm.fit)$sigma
library(car)
vif(lm.fit)

lm.fit1=lm(medv~.-age ,data=Boston )
summary(lm.fit1)

# interaction
summary(lm(medv~lstat*age ,data=Boston))


# nonlinear transform
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit=lm(medv~lstat)

anova(lm.fit ,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)

summary(lm(medv~log(rm),data=Boston))


attach(Carseats)
names(Carseats)

lm.fit=lm(Sales~. + Income:Advertising + Price:Age, data=Carseats )
summary(lm.fit)

# dummy variable
contrasts(ShelveLoc)





# logistic regression
library(ISLR)
names(Smarket)


dim(Smarket)
cor(Smarket[,-9])
plot(Volume)


glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

summary(glm.fit)$coef

glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"

# confusion matrix
table(glm.pred, Direction)

# out of sample prediction
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]


glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)

glm.probs=predict(glm.fit, Smarket.2005, type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005)

mean(glm.pred==Direction.2005)


# principal components analysis 

states=row.names(USArrests)
states
names(USArrests)

apply(USArrests , 2, mean)
apply(USArrests , 2, var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation
dim(pr.out$x)

biplot (pr.out , scale =0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

pr.out$sdev
pr.var=pr.out$sdev ^2
pr.var

# how much of variance is explained by each factor 
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')


# knn k-means clustering

set.seed(2)
x=matrix(rnorm (50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

km.out=kmeans (x,2, nstart =20)
km.out$cluster

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

set.seed(4)
km.out=kmeans(x,3, nstart=20)
km.out


# hierarchical clustering

hc.complete=hclust(dist(x), method="complete")
hc.average =hclust(dist(x), method ="average")
hc.single=hclust(dist(x), method ="single")

par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

cutree(hc.single, 4)

xsc=scale(x)

plot(hclust(dist(xsc), method ="complete "), main=" Hierarchical Clustering with Scaled Features ")

 x=matrix(rnorm (30*3), ncol=3)

dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method ="complete "), main=" Complete Linkage with Correlation -Based Distance ", xlab="", sub ="")





library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

dim(nci.data)

nci.labs[1:4]

table(nci.labs)

pr.out=prcomp(nci.data , scale=TRUE)

Cols=function (vec){
+ cols=rainbow (length(unique(vec)))
+ return(cols[as.numeric (as.factor(vec))])
+ }






# decision trees

library(tree)
library(ISLR)
attach(Carseats)

High = ifelse(Sales <=8, "No", "Yes")

Carseats = data.frame(Carseats, High)

tree.carseats = tree(High~.-Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats


