#Random Forests
#==============

bank_data <- read.table("bank.csv", header=TRUE,sep=";")
str(bank_data)

summary(bank_data)

head(bank_data)

library(psych)

pairs.panels(bank_data[, c(1:8,17)])

pairs.panels(bank_data[, c(9:17)])

new_data <-bank_data[, c(1:4,7:9,12,14,15,17)] 

str(new_data)

pairs.panels(new_data)

new_data$age <- cut(new_data$age, c(1,20,40,60,100))
new_data$is_divorced <- ifelse( new_data$marital == "divorced", 1, 0) 
new_data$is_single <- ifelse( new_data$marital == "single", 1, 0) 
new_data$is_married <- ifelse( new_data$marital == "married", 1, 0) 
new_data$marital <- NULL
str(new_data)

par(mfrow=c(2,2),las=2)
plot( new_data$housing, new_data$y,xlab="Housing", ylab="Become Customer?", col=c("darkgreen","red"))
plot( new_data$contact, new_data$y,xlab="Contact Type", ylab="Become Customer?", col=c("darkgreen","red"))
boxplot( duration ~ y, data=new_data,col="blue") 
boxplot( pdays ~ y, data=new_data,col="maroon")

library(caret)

inTrain <- createDataPartition(y=new_data$y ,p=0.7,list=FALSE) 
training <- new_data[inTrain,]
testing <- new_data[-inTrain,]
dim(training);dim(testing)

table(training$y); table(testing$y)

library(randomForest)

 model <- randomForest(y ~ ., data=training) 
 model

 importance(model)

library(caret)
predicted <- predict(model, testing) 
table(predicted)

confusionMatrix(predicted, testing$y)

accuracy=c()
for (i in seq(1,50, by=1)) {
	modFit <- randomForest(y ~ ., data=training, ntree=i)
	accuracy <- c(accuracy, confusionMatrix(predict(modFit, testing, type="class"), testing$)$overall[1])
}
par(mfrow=c(1,1))
plot(x=seq(1,50, by=1), y=accuracy, type="l", col="red", main="Effect of increasing tree size", xlab="Tree Size", ylab="Accuracy")