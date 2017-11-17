cancer_data <- read.csv("breast_cancer.csv")
str(cancer_data)
head(cancer_data)


library(psych)
pairs.panels(cancer_data[, c(2,3:10)])
pairs.panels(cancer_data[, c(2,11:20)])
pairs.panels(cancer_data[, c(2,21:32)])


#Get only the first 3 components
final_data <- data.frame(pca_data$x[,1:3]) 
#add diagnosis to the data frame 
final_data$diagnosis <- cancer_data$diagnosis
pairs.panels(final_data)

inTrain <- createDataPartition(y=final_data$diagnosis ,p=0.7,list=FALSE) 
training <- final_data[inTrain,]
testing <- final_data[-inTrain,]
dim(training);dim(testing)

table(training$diagnosis); table(testing$diagnosis)

install.packages("car")
install.packages("nnet")
install.packages("caTools")

library(car)
library(nnet)
library(caTools)


predlist <- c("bagFDA","LogitBoost", "nnet", "svmRadialCost") 
#Create a result data set
results <- data.frame( Algorithm=character(), Duration=numeric(), Accuracy=numeric(), stringsAsFactors=FALSE)
#loop through algorithm list and perform model building and prediction
for (i in 1:length(predlist)) {
	pred <- predlist[i] 
	print(paste("Algorithm = ",pred )) 
	#Measure Time
	startTime <- as.integer(Sys.time())
	#Build model
	model <- train( diagnosis ~ ., data=training, method=pred) #Predict
	predicted <- predict(model, testing)
	#Compare results
	matrix<- confusionMatrix(predicted, testing$diagnosis)
	#Measure end time
	endTime <- as.integer(Sys.time())
	#Store result
	thisresult <- c( as.character(pred), endTime-startTime, as.numeric(matrix$overall[1])) results[i,1] <- pred
	results[i,2] <- endTime-startTime
	results[i,3] <- round(as.numeric(matrix$overall[1]) * 100, 2)
}

 #Print results
results