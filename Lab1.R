#EXERCISE 1

EPI_data <- read.csv(file.choose(),header=T,skip=1)
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
help(stem)


plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI)
qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

DALY
summary(DALY)
fivenum(DALY, na.rm=TRUE)
stem(DALY)
help(hist)
hist(DALY,seq(0.,95.,1.0),prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
qqnorm(DALY)
qqline(DALY)
y<-seq(0,95,1)
qqplot(qt(ppoints(250),df=5), y, xlab = "Q-Q plot for t dsn (DALY)")
qqline(y)

WATER_H
summary(WATER_H)
fivenum(WATER_H,na.rm=TRUE)
stem(WATER_H)
hist(WATER_H,seq(0.,100.,1.0),prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)
z<-seq(0,100,1)
qqplot(qt(ppoints(250),df=5), z, xlab="Q-Q plot for t dsn (WATER_H)")
qqline(z)

boxplot(EPI,DALY,WATER_H)
qqplot(EPI,DALY)
qqplot(WATER_H,DALY)
AIR_H
tf = is.na(AIR_H)
airh = AIR_H[!tf]
boxplot(WATER_H,airh)
qqplot(WATER_H,airh)

#EXERCISE 2
EPILand = EPI[!Landlock]
Eland = EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)
x = seq(30,95,1)
qqplot(qt(ppoints(250),df=5), x, xlab="Q-Q plot for t dsn (Eland)")
qqline(x)

EPIEurope = EPI[EPI_regions=='Europe']
EPISoutheastAsia = EPI[GEO_subregion=='South East Asia']

WATER_H_US = WATER_H[Country=='United States of America']
WATER_H_SUBSAHARAN_AFRICA = WATER_H[EPI_regions=='Sub-Saharan Africa']
hist(WATER_H_SUBSAHARAN_AFRICA)
boxplot(WATER_H_SUBSAHARAN_AFRICA,WATER_H_US)


#other data
GRUMP_data <- read.csv(file.choose(),header=T)
View(GRUMP_data)

PopulationPerUnit = GRUMP_data$PopulationPerUnit
PopulationPerUnit
PPUnit = PopulationPerUnit[!is.na(PopulationPerUnit)]
hist(PPUnit)
fivenum(PPUnit)
summary(PPUnit)
x = seq(0,2658,5)
qqnorm(PPUnit)
qqline(PPUnit)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn (Population Per Unit)")
hist(PPUnit, seq(0.,2660.,5.0),prob=TRUE)
lines(density(PPUnit))


Area = GRUMP_data$Area
Area = as.integer(Area)
Area = Area[!is.na(Area)]
Area
summary(Area)
hist(Area)
hist(Area,breaks=100)
lines(frequency(Area))
rug(Area)
z = seq(1,1668000,100)
qqnorm(Area)
qqline(Area)
qqplot(qt(ppoints(250),df=5),z,xlab="Q-Q plot for t dsn (Area)")

boxplot(Area,PPUnit)
qqplot(Area,PPUnit)

waterTreatment = read.csv(file.choose(), header=T)
View(waterTreatment)
PH_E = waterTreatment$PH.E
PH_E
summary(PH_E)
hist(PH_E)
hist(PH_E, seq(6.5,9.,0.1),prob=TRUE)
plot(ecdf(PH_E),do.points=FALSE,verticals=TRUE)
qqnorm(PH_E)
qqline(PH_E)
x = seq(6.5,9,0.1)
help(qqplot)


SS_P = waterTreatment$SS.P
SS_P
hist(SS_P)
boxplot(SS_P)
plot(ecdf(SS_P),do.points=FALSE,verticals=TRUE)
summary(SS_P)
qqnorm(SS_P)
qqplot(SS_P,PH_E)
