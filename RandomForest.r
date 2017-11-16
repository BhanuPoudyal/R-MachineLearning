bank_data <- read.table("bank.csv", header=TRUE,sep=";")
str(bank_data)

summary(bank_data)

head(bank_data)

#Data Cleaning
#1. The ranges of values in each of the variables (columns) look ok without any kind of outliers 
#2. There is equal distribution of the three classes - setosa, versicolor and virginia

#correlations
library(psych)
pairs.panels(bank_data[, c(1:8,17)])

pairs.panels(bank_data[, c(9:17)])

#Based on the correlation co-efficients, let us eliminate default, balance, day, month, campaign, poutcome because of very low correlation. There are others too with very low correlation, but let us keep it for example sake.

new_data <-bank_data[, c(1:4,7:9,12,14,15,17)] str(new_data)

#Data Transformations
new_data$age <- cut(new_data$age, c(1,20,40,60,100))
new_data$is_divorced <- ifelse( new_data$marital == "divorced", 1, 0) new_data$is_single <- ifelse( new_data$marital == "single", 1, 0) new_data$is_married <- ifelse( new_data$marital == "married", 1, 0) new_data$marital <- NULL
str(new_data)

par(mfrow=c(2,2),las=2)
plot( new_data$housing, new_data$y,
xlab="Housing", ylab="Become Customer?", col=c("darkgreen","red"))
plot( new_data$contact, new_data$y,
xlab="Contact Type", ylab="Become Customer?", col=c("darkgreen","red"))
boxplot( duration ~ y, data=new_data,col="blue") boxplot( pdays ~ y, data=new_data,col="maroon")

#Model Building
library(caret)

inTrain <- createDataPartition(y=new_data$y ,p=0.7,list=FALSE) training <- new_data[inTrain,]
testing <- new_data[-inTrain,]
dim(training);dim(testing)

table(training$y); table(testing$y)

install.packages("randomForest")
library(randomForest)

model <- randomForest(y ~ ., data=training)
importance(model)

 library(caret)
predicted <- predict(model, testing) 
table(predicted)
confusionMatrix(predicted, testing$y)


#Effect of increasing tree count
#Let us try to build different number of trees and see the effect of that on the accuracy of the prediction

accuracy=c()
for (i in seq(1,50, by=1)) {
modFit <- randomForest(y ~ ., data=training, ntree=i)
accuracy <- c(accuracy, confusionMatrix(predict(modFit, testing, type="class"), testing$ }
par(mfrow=c(1,1))
plot(x=seq(1,50, by=1), y=accuracy, type="l", col="red",
     main="Effect of increasing tree size", xlab="Tree Size", ylab="Accuracy")

     

