set.seed(12345)
help(par)

par(mar = rep(0.2,4))
help("rnorm")
??rnorm

data_Matrix <- matrix(rnorm(400), nrow=40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

par(mar = rep(0.2,4))
heatmap(data_Matrix)

set.seed(678910)
for (i in 1:40){
  coin_Flip <- rbinom(1, size=1, prob=0.5)
  if (coin_Flip){
    data_Matrix[i,] <- data_Matrix[i, ] + rep(c(0,3), each=5)
  }
}
par(mar = rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
heatmap(data_Matrix)

hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab="The Row Mean", ylab="Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab="Column", ylab="Column Mean", pch=19)

#---------knn-abalone-----------

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),header=FALSE,sep=",")
colnames(abalone) <- c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings")
summary(abalone)
str(abalone)
summary(abalone$rings)

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels= c("young","adult","old"))
abalone$rings <- as.factor(abalone$rings)

summary(abalone$rings)

z <- abalone
aba <- abalone
aba$sex <- NULL

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

aba[1:7] = as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_weight)

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7,0.3))

KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]

sqrt(2918)

library(class)
KNNpred <- knn(train=KNNtrain[1:7],test=KNNtest[1:7],cl=KNNtrain$rings,k=55)
KNNpred
table(KNNpred)

#--------iris clustering---------

sapply(iris[,-5],var)
set.seed(300) #generates random values a certain way
k.max <- 5
wss <- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=1000)$tot.withinss})
wss
plot(1:k.max,wss,type="b",xlab="Number of clusters(k)",ylab="Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart=20)
table(iris[,5],icluster$cluster)
