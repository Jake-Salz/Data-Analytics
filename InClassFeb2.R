multivariate <- read.csv("../Downloads/multivariate.csv")
attach(multivariate)
names(multivariate)
multivariate

plot(Income,Immigrant,main = "Scatterplot")
plot(Immigrant,Homeowners)


help(lm)
mm <- lm(Homeowners ~ Immigrant)
mm
plot(Immigrant,Homeowners)
abline(mm)
abline(mm, col=2, lwd=3)

summary(mm)
attributes(mm)
mm$coefficients


HP<-Homeowners/Population
PD<-Population/area
mm<-lm(Immigrant~Income+Population+HP+PD)
lm(formula=Immigrant~Income+Population+HP+PD)
cm<-coef(mm)
cm
