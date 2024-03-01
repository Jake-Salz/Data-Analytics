cars
dim(cars)
cars1 = cars[1:30,]
cars_outliers <- data.frame(speed=c(19,19,20,20,21), dist=c(190,186,210,220,218))
head(cars_outliers)
cars2 <- rbind(cars1, cars_outliers)
help(par)

par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, xlim=c(0,28), ylim=c(0,230), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

plot(cars1$speed, cars1$dist, xlim=c(0,28), vlim=c(0,230), main="Outliers Removed", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)
