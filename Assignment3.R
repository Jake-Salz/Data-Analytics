library('xlsx')
nyt4 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt4.csv',header=TRUE)
nyt6 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt6.csv',header=TRUE)
nyt9 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt9.csv',header=TRUE)
nyt10 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt10.csv',header=TRUE)
nyt12 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt12.csv',header=TRUE)
nyt13 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt13.csv',header=TRUE)
nyt22 <- read.csv('C:/users/salzj/OneDrive/Data Analytics/nytimes-selected/nyt22.csv',header=TRUE)

#Running summary statistics on each variable of nyt4
impressions4 <- nyt4[,'Impressions']
clicks4 <- nyt4[,'Clicks']
age4 <- nyt4[,'Age']
signed_in4 <- nyt4[,'Signed_In']
gender4 <- nyt4[,'Gender']

summary(impressions4)
summary(clicks4)
summary(age4)
summary(gender4)

#Will use variables clicks and impressions,
#creating variables for all datasets
clicks6 <- nyt6[,'Clicks']
impressions6 <- nyt6[,'Impressions']
summary(clicks6)
summary(impressions6)

clicks9 <- nyt9[,'Clicks']
impressions9 <- nyt9[,'Impressions']
summary(clicks9)
summary(impressions9)

clicks10 <- nyt10[,'Clicks']
impressions10 <- nyt10[,'Impressions']
summary(clicks10)
summary(impressions10)

clicks12 <- nyt12[,'Clicks']
impressions12 <- nyt12[,'Impressions']
summary(clicks12)
summary(impressions12)

clicks13 <- nyt13[,'Clicks']
impressions13 <- nyt13[,'Impressions']
summary(clicks13)
summary(impressions13)

clicks22 <- nyt22[,'Clicks']
impressions22 <- nyt22[,'Impressions']
summary(clicks22)
summary(impressions22)

#create boxplots for all datasets
boxplot(impressions4,clicks4, main="Clicks and Impressions for NYT4", names=c("Impressions","Clicks"))
boxplot(impressions6,clicks6, main="Clicks and Impressions for NYT6", names=c("Impressions","Clicks"))
boxplot(impressions9,clicks9, main="Clicks and Impressions for NYT9", names=c("Impressions","Clicks"))
boxplot(impressions10,clicks10, main="Clicks and Impressions for NYT10", names=c("Impressions","Clicks"))
boxplot(impressions12,clicks12, main="Clicks and Impressions for NYT12", names=c("Impressions","Clicks"))
boxplot(impressions13,clicks13, main="Clicks and Impressions for NYT13", names=c("Impressions","Clicks"))
boxplot(impressions22,clicks22, main="Clicks and Impressions for NYT22", names=c("Impressions","Clicks"))

#conduct Anderson Darling tests for all variables
install.packages('nortest')
ad.test(impressions4)
ad.test(clicks4)
ad.test(impressions6)
ad.test(clicks6)
ad.test(impressions9)
ad.test(clicks9)
ad.test(impressions9)
ad.test(clicks10)
ad.test(impressions10)
ad.test(clicks12)
ad.test(impressions12)
ad.test(clicks13)
ad.test(impressions13)
ad.test(clicks22)
ad.test(impressions22)

#create histograms for all of the datasets
hist(clicks4)
hist(impressions4)
hist(clicks6)
hist(impressions6)
hist(clicks9)
hist(impressions9)
hist(clicks10)
hist(impressions10)
hist(clicks12)
hist(impressions12)
hist(clicks13)
hist(impressions13)
hist(clicks22)
hist(impressions22)

#ECDF and QQ plots
summary(impressions4)
plot(ecdf(impressions4), do.points=FALSE, verticals=TRUE)
poisson_data <- rpois(length(impressions4), lambda=5)
hist(poisson_data)
qqplot(impressions4, poisson_data)
abline(0,1,col="red")
plot(ecdf(clicks4), do.points=FALSE, verticals=TRUE)
poisson_clicks <- rpois(length(clicks4), lambda=0.25)
hist(poisson_clicks)
qqplot(clicks4, poisson_clicks)
abline(0,1, col="red")

#Kolmogorov-Smirnov Test
ks_test_impressions <- ks.test(impressions4, poisson_data)
print(ks_test_impressions)
ks_test_clicks <- ks.test(clicks4, poisson_clicks)
print(ks_test_clicks)
