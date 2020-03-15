library(ggplot2)
library(rpart)
library(rpart.plot)
msleep <- data.frame(msleep)
str(msleep)
help("msleep")
# creating a new data frame with the following columns included.
msleepdf1 <- msleep[,c(3,6,10,11)] 
# 3 = vore ,6=sleep_total, 10=brainwt, 11=bodywt
# observe the structure of the msleepdf1
str(msleepdf1)
head(msleepdf1)
# Building Regression Decision Tree that predicts the total sleeping
# hours of the mamals based on the other #variables available on the dataset
help("rpart") 
sleepModel_1 <- rpart(sleep_total ~ ., data=msleepdf1, method = "anova")
# method we are using here is anova becuase our target here is sleep_total is a numerical one.
sleepModel_1
# visualize this using rpart.plot()
help("rpart.plot")
rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)
# type = 3, Draw separate split labels for the left and right directions.See the documentation 
#fallen.leaves = TRUE,  Default TRUE to position the leaf nodes at the bottom of the graph. 
#It can be helpful to use FALSE if the graph is too crowded and the text size is too small.
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves = TRUE) # with 3 digits 
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves = TRUE)



# instrall the C50 package
install.packages("C50")
require(C50)
# we will be using the iris dataset to do a classfication
data("iris")
head(iris)
str(iris)
table(iris$Species)
# set the seed
set.seed(9850)
# generate random numbers
grn <-runif(nrow(iris))
# creating a randomized iris dataset ,  shuffling the dataset
irisrand <-iris[order(grn),]
str(irisrand)
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1
# use the confusion matrix to understand prediction
table(irisrand[101:150,5],prediction1)
# plot the classification model tree
plot(classificationmodel1)




# Call the NaiveBayes Classifier Package e1071, which auto calls the Class package
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 

