myData = read.csv(file.choose(),header=T)
View(myData)

myData$EPI
summary(myData$EPI)
boxplot(myData$EPI)
fivenum(myData$EPI,na.rm=TRUE)
hist(myData$EPI)

stem(myData$EPI)
hist(myData$EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(myData$EPI,na.rm=T,bw="SJ"))
rug(myData$EPI)


plot(ecdf(myData$EPI),do.points=F,verticals=T)
par(pty="s")
qqnorm(myData$EPI)
qqline(myData$EPI)
x = seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)
DALY = myData$DALY
EPI = myData$EPI
boxplot(EPI,DALY)
qqplot(EPI,DALY)

help(data.frame)
