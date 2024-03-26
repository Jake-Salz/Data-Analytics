data("iris")
head(iris)
irisdata1 <- iris[,1:4]
irisdata1

help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
plot(principal_components)
help(biplot)
biplot(principal_components)

install.packages('MASS')
data(Boston, package="MASS")
help(Boston)

pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)
biplot(pca_out, scale=0)
boston_pc <- pca_out$x
boston_pc
summary(boston_pc)
