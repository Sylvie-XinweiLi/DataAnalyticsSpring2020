#Lab2 part-a
#read data
help("read.csv")
EPI_data<-read.csv(file.choose(),header = TRUE,skip=1)
#Generate Central Tendency values for EPI variable
#Generate Central Tendency values for DALY variable
attach(EPI_data)
summary(EPI)
stem(EPI)
summary(DALY)
stem(DALY)
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
print(getmode(EPI))
print(getmode(DALY))
#Generate the Histogram for EPI variable
#Generate the Histogram for DALY variable
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
hist(DALY)
hist(DALY,seq(0.,110.,2.0),prob=TRUE)

#Dplyr exercises 
install.packages('dplyr')
library(dplyr)
#Using sample_n() function in dplyr, get 5 random data points From EPI, DALY 
EPI_data<-na.omit(EPI_data)
attach(EPI_data)
epi<-data.frame(EPI)
daly<-data.frame(DALY)
sample_n(epi, 5,replace = FALSE) 
sample_n(daly, 5)
#Using sample_frac() function in dplyr, get 10% random data points From EPI, DALY 
sample_frac(epi, 0.1) 
sample_frac(daly, 0.1)
#Use the arrange() and desc() functions to arrange values in the descending order in the EPI and DALY  
#and assign them to new variables:new_decs_EPI and new_decs_DALY
new_decs_EPI <- arrange( EPI_data ,desc(EPI))
new_decs_DALY <- arrange( EPI_data ,desc(DALY))
#Using the mutate() function, create new columns:
#double_EPI and double_DALY where multiplying the values in EPI and DALY by 2 
EPI_data_new<-mutate(EPI_data,double_EPI= EPI*2)
EPI_data_new<-mutate(EPI_data,double_DALY= DALY*2)
#Using the summarise() function along with the mean() function to find the mean for EPI and DALY
summarise(EPI_data, avg_EPI = mean(EPI, na.rm = TRUE)) 
summarise(EPI_data, avg_DALY = mean(DALY, na.rm = TRUE)) 
#boxplot(ENVHEALTH,ECOSYSTEM)
boxplot(ENVHEALTH,ECOSYSTEM)
#qqplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)

#Lab2 part1-b Regression Exercises
#Using the EPI find the single most important factor in increasing the EPI in a given region
EPI_EUROPE<-filter(EPI_data,EPI_regions=="Europe")
str(EPI_EUROPE)
EPI_EUROPE_NUM <- subset(EPI_EUROPE,select=-c(2,3,4,5))
EPI_EUROPE_new <- sapply(EPI_EUROPE_NUM, as.numeric)
COR <- data.frame(round(cor(EPI_EUROPE_new),2))
COR$EPI
#we can observe that the maximum correlation is 0.92,so "ECOSYSTEM" is the most important factor.

#Linear and least-squares
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
#Predict
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
NEW
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")


#Lab2 part-b
#Exercise1: Regression 
help("read.csv")
MulReg_data<-read.csv(file.choose(),header = TRUE)
attach(MulReg_data)
boxplot(ROLL,UNEM,HGRAD)
lmENVH<-lm(ROLL~UNEM+HGRAD)
pROLL<- predict(lmENVH,data.frame(UNEM=0.07,HGRAD=90000))
pROLL 
# prediction of ROLL equals to 76598.04
boxplot(ROLL,UNEM,HGRAD,INC)
lmENVH<-lm(ROLL~UNEM+HGRAD+INC)
pROLL<- predict(lmENVH,data.frame(UNEM=0.07,HGRAD=90000,INC=25000))
pROLL
# prediction of ROLL equals to 134333.2

#Exercise 2: Classification 
help("read.csv")
abalone<-read.csv(file.choose(),header = TRUE)
str(abalone)
summary(abalone$Rings)

abalone$Rings <- as.numeric(abalone$Rings)
abalone$Rings <- cut(abalone$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$Rings <- as.factor(abalone$Rings)
summary(abalone$Rings)
# remove the "sex" varialbe in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$Sex <-NULL
#  normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$Shucked.weight)
# After Normalization, each variable has a min of 0 and a max of 1.
# in otherwords, values are in the range from 0 to 1.
# We'll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852  round it to 55 and use k = 55 # We usually take an Odd number for k value, 
# knn model 
# knn() is in the "class" library.Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio. 
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$Rings, k = 55)
KNNpred
table(KNNpred)

#Exercise 3: Clustering 
data("iris")
iris_new <- iris[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
set.seed(300)
k.max <- 12
# tot.withinss = Total within-cluster sum of square 
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(iris[,1:4],k,nstart = 10,iter.max = 1000)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,1:4],3,nstart = 10)
table(icluster$cluster,iris$Species)
# In the table we can see that most of the observations have been clustered correctly 
# however, 2 of the versicolor have been put in the cluster with the virginica. 
# and 14 of the versicolor have been put in cluster 2 which mostly has versicolor.
