 #install.packages("psych")
 library(psych)
 pairs.panels(iris_data)
 #split data and train model
 #install.packages("caret")
 library(caret)
 inTrain <- createDataPartition(y=iris_data$Species, p=0.7, list=FALSE)
 summary(inTrain)
   
 str(inTrain)
 
 training <- iris_data[inTrain, ]
 testing <- iris_data[-inTrain, ]
 dim(training)

 dim(testing)

 table(training$Species)
 table(testing$Species)
 #install.packages("C50")
 library(C50)
 model <- C5.0(training[-5], y=training$Species)
 summary(model)

 #install.packages("caret")
 library(caret)
 predicted <- predict(model, testing)
 predicted
 table(predicted)
 table(iris_data)
 table(iris_data$Species)
 confusionMatrix(predicted, testing$Species)
