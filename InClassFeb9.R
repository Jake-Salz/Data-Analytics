library(rpart)
library(rpart.plot)


iris
dim(iris)


s_iris <- sample(150,100)
s_iris

iris_train <- iris[s_iris,]
iris_test <- iris[-s_iris,]
dim(iris_test)
dim(iris_train)

detectionTreeModel <- rpart(Species~., iris_train, method= "class")
detectionTreeModel
rpart.plot(detectionTreeModel)
