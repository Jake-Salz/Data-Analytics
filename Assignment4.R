library('xlsx')
library('dplyr')
citywide_data <- read.csv('C:/users/salzj/OneDrive/Data Analytics/citywide-housing/NYC_Citywide_Annualized_Calendar_Sales_Update_20240405.csv')

manhattan_data <- citywide_data[citywide_data$BOROUGH==1,]
manhattan_data_cleaned <- manhattan_data[manhattan_data$SALE.PRICE>10000,]
dim(manhattan_data_cleaned)
manhattan_data_reasonable <- manhattan_data_cleaned[manhattan_data_cleaned$SALE.PRICE<10000000,]
summary(manhattan_data_reasonable$SALE.PRICE)
boxplot(manhattan_data_reasonable$SALE.PRICE)
hist(manhattan_data_reasonable$SALE.PRICE, 
     breaks = seq(min(manhattan_data_reasonable$SALE.PRICE), 
                  max(manhattan_data_reasonable$SALE.PRICE) + 10000, 
                  by = 10000), 
     main = "Histogram of Sale Prices in Manhattan",
     xlab = "Sale Price",
     ylab = "Frequency",
     col = "blue",
     border = "black")
manhattan_data_lm <- subset(manhattan_data_cleaned, select=c(BUILDING.CLASS.CATEGORY,NEIGHBORHOOD,ZIP.CODE,GROSS.SQUARE.FEET,SALE.PRICE))
manhattan_data_lm = manhattan_data_lm[1:10000,]
sales_price_predict <- lm(manhattan_data_lm$SALE.PRICE~., data=manhattan_data_lm)
summary(sales_price_predict)

cooksD <- cooks.distance(sales_price_predict)
influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm=TRUE)))]
influential_prices <- names(influential)
influential_prices
outliers <- manhattan_data_lm[influential_prices,]
manhattan_lm_no_outliers <- manhattan_data_lm %>% anti_join(outliers) 
sales_price_predict2 <- lm(manhattan_lm_no_outliers$SALE.PRICE~., data=manhattan_lm_no_outliers)
summary(sales_price_predict2)
summary(manhattan_data_cleaned$SALE.PRICE)

#columns for multivariate regression
sample1_regression = manhattan_data_cleaned[1:10000,]                  
sample2_regression = manhattan_data_cleaned[15000:25000,]
sample3_regression = manhattan_data_cleaned[45000:55000,]
sample1_lm <- lm(sample1_regression$SALE.PRICE~sample1_regression$GROSS.SQUARE.FEET+sample1_regression$LAND.SQUARE.FEET, data=sample1_regression )
sample2_lm <- lm(sample2_regression$SALE.PRICE~sample2_regression$GROSS.SQUARE.FEET+sample2_regression$LAND.SQUARE.FEET, data=sample2_regression )
sample3_lm <- lm(sample3_regression$SALE.PRICE~sample3_regression$GROSS.SQUARE.FEET+sample3_regression$LAND.SQUARE.FEET, data=sample3_regression )
summary(sample1_lm)
summary(sample2_lm)
summary(sample3_lm)

library("ggplot2")
ggplot(manhattan_data, aes(x=manhattan_data$GROSS.SQUARE.FEET,y=manhattan_data$SALE.PRICE)) + geom_point()
ggplot(manhattan_data, aes(x=manhattan_data$SALE.PRICE)) + geom_histogram(binwidth=10000)

#generating knn-clustering
manhattan_data_cleaned$GROSS.SQUARE.FEET <- as.numeric(manhattan_data_cleaned$GROSS.SQUARE.FEET)
manhattan_data_cleaned <- manhattan_data_cleaned[!is.na(manhattan_data_cleaned$GROSS.SQUARE.FEET),]
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(X)))
}
manhattan_data_cleaned$GROSS.SQUARE.FEET <- cut(manhattan_data_cleaned$GROSS.SQUARE.FEET, br=c(-1,333,666,999), labels=c("small","medium","large"))
manhattan_data_cleaned$GROSS.SQUARE.FEET <- as.factor(manhattan_data_cleaned$GROSS.SQUARE.FEET)
manhattan_data_cleaned$SALE.PRICE <- as.numeric(manhattan_data_cleaned$SALE.PRICE)
#use numeric data columns - gross square feet, land square feet, 





help("scale")
sale_price_prepared[2:6] <- as.data.frame(lapply(sale_price_prepared[2:6],normalize))
sale_price_normalized$SALE.PRICE
ind <- sample(2, nrow(sale_price_prepared),replace=TRUE,prob=c(0.7,0.3))
KNNTrain <- sale_price_prepared[ind==1,]
KNNTest <- sale_price_prepared[ind==2,]
KNNTrain[1:6]
library("class")
KNNPred <- knn(train=KNNTrain[1:6],test=KNNTest[1:6],cl=KNNTrain$GROSS.SQUARE.FEET,k=173)
