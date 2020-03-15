#creating a dataframe
#Example: RPI Weather dataframe
days<-c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp<-c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed<-c('T','T','F','F','T','T','F')
help("data.frame")
RPI_Weather_Week<-data.frame(days,temp,snowed)

RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)#why 0 rows

sorted.snowed<-order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]
#RPI_Weather_Week[descending_snowed,]
dec.snow<-order(-RPI_Weather_Week$temp)
dec.snow

#creating an empty dataframe
empty.DataFrame<-data.frame()
v1<-1:10
v1
letters
v2<-letters[1:10]
df<-data.frame(col.name.1=v1,col.name.2=v2)
df
#write to a CSV file
write.csv(df,file='saved_df1.csv')
df2<-read.csv('saved_df1.csv')
df2

help("read.csv")
EPI2010<-read.csv(file.choose(),header = TRUE,skip=1)
data()
help(data)
data(cars)
View(EPI2010)
attach(EPI2010)
fix(EPI2010) 
EPI 
tf <- is.na(EPI) 
E <- EPI[!tf]
#exercise1 exploring the distribution
summary(EPI)
fivenum(EPI,na.rm = TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm = TRUE,bw=1.))
rug(EPI)
#exercise1 fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(DALY); qqline(DALY)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for DALY")
qqline(x)

plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(AIR_H); qqline(AIR_H)
x<-seq(30,100,2)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for AIR_H")
qqline(x)

#Comparing distributions
boxplot(EPI,DALY)
qqplot(EPI,DALY)
boxplot(ECOSYSTEM,WATER_H)
qqplot(ECOSYSTEM,WATER_H)
boxplot(AIR_E,WATER_E)
qqplot(AIR_E,WATER_E)
help("distributions")

#Exercise 2: filtering (populations)
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
EPI_South_Asia <- EPI2010[EPI_regions=="South Asia",]

#GRUMP - repeat
help("read.csv")
GRUMP_data<-read.csv(file.choose(),header = TRUE)
summary(GRUMP_data)
attach(GRUMP_data)
fivenum(CountryCodeLong,na.rm = TRUE)
stem(CountryCodeLong)
hist(CountryCodeLong)
hist(CountryCodeLong,prob=TRUE)
lines(density(CountryCodeLong,na.rm = TRUE))
rug(CountryCodeLong)
#fitting a distribution beyond histograms
plot(ecdf(CountryCodeLong), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(CountryCodeLong); qqline(CountryCodeLong)
x<-seq(1000,10000,500)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
#Comparing distributions
boxplot(CountryCodeLong,LevelUsed)
qqplot(CountryCodeLong,LevelUsed)
boxplot(Resolution,LevelUsed)
qqplot(Resolution,LevelUsed)
#filtering (populations)
Asia<-ContinentName["Asia"]
hist(Continent)

#water_treatment - repeat
help("read.csv")
Water<-read.csv(file.choose(),header = TRUE)
summary(Water)
attach(Water)
fivenum(PH.E,na.rm = TRUE)
stem(PH.E)
hist(PH.E)
hist(PH.E,prob=TRUE)
lines(density(PH.E,na.rm = TRUE))
rug(PH.E)
#fitting a distribution beyond histograms
plot(ecdf(PH.E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(PH.E); qqline(PH.E)
x<-seq(6,9,0.1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
#Comparing distributions
boxplot(COND.E,PH.E)
qqplot(COND.E,PH.E)

