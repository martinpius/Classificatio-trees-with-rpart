#In this project we fits various classification models 
rm(list = ls())
head(iris)
str(iris)
table(iris$Species)
#Plot the scatter plot
boxplot(iris$Sepal.Length~iris$Species)
plot(iris)
#KNN algorthim in R
#Classification modeling using nearest neighbour algorithim
#Data preparation
#Reshaffling iris dataset (Randomized the raws)
set.seed(9850)
Tag=runif(nrow(iris))
Tag
Iris2=iris[order(Tag),]
Iris2
str(Iris2)
#Check summary for all numeric variables
summary(Iris2[,c(1,2,3,4)])
#Standardize all numeric variables to the range (0,1)
Stand1=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
Stand1(c(1,4,6))
Stand1(c(10,20,30,60))
#Creating a new dataset with standerdized all numerical values
Irisa=as.data.frame(lapply(Iris2[,c(1,2,3,4)], Stand1))
Irisa
summary(Irisa)
#K nearest neighbour algorithim to predict species for iris dataset
#Creating training and test dataset.
#Test dataset is at leat 10% of total obs
str(Irisa)
mytrain=Irisa[1:129,]
mytest=Irisa[130:150,]
target_train=Iris2[1:129,5]
target_test=Iris2[130:150,5]

#Load The package class
require(class)
m1=knn(train = mytrain,test = mytest,cl=target_train,k=13)
m1
#Confusion matrix
table(m1,target_test)

#Prediction of iris dataset using classification tree
install.packages("C50")
require(C50)
#Reshaffling and partition of iris dataset
iris
set.seed(9850)
g=runif(nrow(iris))
iris_r=iris[order(g),]
#Classification tree model using C50
m2=C5.0(iris_r[1:100,-5],iris_r[1:100,5])
m2
summary(m2)
#Predict my target
p1=predict(m2,iris_r[101:150,])
p1
#Confusion matrix
table(iris_r[101:150,5],p1)
plot(m2)

#Classification tree using rpart.
install.packages("rpart")
install.packages("rpart.plot")
require(rpart)
require(rpart.plot)
#Building the model using the training dataset
m3=rpart(Species~., data = iris_r[1:100,])
m3
rpart.plot(m3,type = 3,fallen.leaves = T)
#Model testing?//Predictin
p4=predict(m3,iris_r[101:150,],type = "class")
p4
table(iris_r[101:150,5],p4)

library(rpart)
library(rpart.plot)
data()
head(WWWusage)
str(WWWusage)
str(sleep)
str(ChickWeight)
head(chickwts)
head(ChickWeight)
str(ChickWeight)
ChickWeight$Chick=as.factor(ChickWeight)
Chicken=ChickWeight[,c(1,2,4)]
Chicken
head(Chicken)
table(Chicken$Diet)
plot(Chicken)
#Predicting chicken weight using regression tree model
#Selecting training and testing data
train1=Chicken[1:500,]
test1=Chicken[501:578,]
m8=rpart(weight~.,data = train1,method = "anova")
m8
summary(m8)
rpart.plot(m8,type = 3,fallen.leaves = T)
p10=predict(m8,test1)
p10
#How better is our model
MAE=function(actual,predicted){
  mean(abs(actual-predicted))
}
MAE(test1$weight,p10)

head(msleep)
#Lower case everthing


library(ggplot2)
data()
head(msleep)
#Classification using KNN
#Refining the data
str(msleep)
Animal=msleep[,c(3,5,6,7,8,9,10,11)]
Animal
train30=Animal[1:60,]
Test30=Animal[60:83,]
m30=rpart(sleep_total~vore+brainwt+bodywt, data = train30,method = "anova")
m30
rpart.plot(m30,type = 3,fallen.leaves = T)
p30=predict(m30,Test30)
p30
MAE(Test30$sleep_total,p30)
