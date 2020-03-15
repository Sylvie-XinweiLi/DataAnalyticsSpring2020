#Exercise 1: fitting a distribution beyond histograms
help("read.csv")
EPI_data<-read.csv(file.choose(),header = TRUE)
data()
View(EPI_data)
attach(EPI_data)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
help("qqnorm")
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) 
par(pty="s")
help("qqnorm") 
help("qqplot")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

#Linear basis and least-squares constraints
help("read.csv")
multivariate<-read.csv(file.choose(),header = TRUE)
attach(multivariate)
help(lm)
mm<-lm(Homeowners~Immigrant)
mm
head(multivariate)
summary(mm)$coef 

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

#Chapter2--R Graphics Cookbook
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature, y=pressure))+geom_line()+geom_point()

#Creating Bar Graphs
barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#creating histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10)
hist(mtcars$mpg,breaks = 5)
hist(mtcars$mpg,breaks = 12)
qplot(mpg,aes(x=mpg))+geom_histogram(binwidth=4)
qplot(mpg,aes(x=mpg))+geom_histogram(binwidth=5)

#creating box-plot
plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
qplot(supp,len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
qplot(interaction(supp,dose),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()

# Dplyr for Data Manipulating--code writing
install.packages('dplyr')
install.packages('nycflights13')
library(dplyr)
library(nycflights13)
head(flights)
summary(flights)

filter(flights,month == 10, day == 4, carrier =='AA')
head(filter(flights, month == 10, day == 4, carrier == 'AA'))
head(flights[flights$month == 10 & flights$day == 4 & flights$carrier == 'AA' , ]) 

slice(flights, 1:15)
arrange(flights,year,month,day, arr_time)
head(arrange(flights,year,month,day,arr_time))
head(arrange(flights,year,month,day, desc(arr_time)))

select(flights,carrier)
head(select(flights,carrier))
head(select(flights, carrier, arr_time))
head(select(flights, carrier, arr_time, day))
head(rename(flights, airline.carrier = carrier))

distinct(select(flights, carrier))
head(mutate(flights, MyNewColumn = arr_delay - dep_delay))
head(transmute(flights, MyNewColumn = arr_delay - dep_delay))

summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE)) 
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE))

sample_n(flights, 15) 
sample_n(flights, 71)  

sample_frac(flights,0.1) 
sample_frac(flights, 0.3) 
sample_n(flights, 30)
sample_frac(flights, 0.5)
library(dplyr)
df_mtcars <- mtcars
head(df_mtcars)

filter(df_mtcars, mpg > 20) 
sample_n(filter(df_mtcars, mpg > 20), 10)
arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg <- arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg

# Dplyr for Data Manipulating--using data multivariate
library(dplyr)
head(multivariate)
summary(multivariate)

filter(multivariate,area>70000,Income>40000)
head(multivariate[multivariate$area>70000 & multivariate$Income>40000, ]) 

slice(multivariate, 1:3)
arrange(multivariate,area,Income)
arrange(multivariate,area,desc(Income))

select(multivariate,area)
select(multivariate,area,Income)
rename(multivariate,Area= area)

distinct(select(multivariate,area))
head(mutate(multivariate, MyNewColumn = Income/Population))

summarise(multivariate, avg_income = mean(Income, na.rm = TRUE)) 
summarise(multivariate, avg_income = sum(Income, na.rm = TRUE))

sample_n(multivariate, 4) 
sample_frac(multivariate, 0.3) 




