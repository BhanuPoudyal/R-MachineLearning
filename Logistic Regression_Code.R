#Import Product Sales Data 
Product_sales <- read.csv("D:\\Google Drive\\Training\\Datasets\\Product Sales Data\\Product_sales.csv")

dim(Product_sales)
names(Product_sales)

table(Product_sales$Bought)

#Building a linear regression line
prod_sales_model<-lm(Bought~Age,data=Product_sales)
summary(prod_sales_model)

#Prediction for Age=4
new_data<-data.frame(Age=4)
predict(prod_sales_model,new_data)

#Prediction for Age=105
new_data<-data.frame(Age=105)
predict(prod_sales_model,new_data)

#Plotting data and Linear Regression line
plot(Product_sales$Age,Product_sales$Bought,col = "blue", xlab="Age", ylab="Buy")
abline(prod_sales_model, lwd = 5, col="red")



############################################################
##Building logistic Regression Line

prod_sales_Logit_model <- glm(Bought ~ Age,family=binomial(),data=Product_sales)
summary(prod_sales_Logit_model)

#Prediction for Age=3
new_data<-data.frame(Age=4)
predict(prod_sales_Logit_model,new_data,type="response")

#Prediction for Age=105
new_data<-data.frame(Age=105)
predict(prod_sales_Logit_model,new_data,type="response")

#Plotting the regression lines
plot(Product_sales$Age,Product_sales$Bought,col = "blue")
curve(predict(prod_sales_Logit_model,data.frame(Age=x),type="resp"),add=TRUE, lwd = 5, col = "blue")
#Adding linear Regression Line
abline(prod_sales_model, lwd = 5, col="red")


############################################################
##Multiple logistic Regression Line

Fiberbits <- read.csv("D:\\Google Drive\\Training\\Datasets\\Fiberbits\\Fiberbits.csv")


Fiberbits_model_1<-glm(active_cust~.,family=binomial(),data=Fiberbits)
summary(Fiberbits_model_1)


###########Classification Table
predicted_values<-predict(prod_sales_Logit_model,type="response")
cat("Predcited Values")
predicted_values[1:10]

cat("Lets convert them to classes using a threshold")
threshold=0.5
threshold

predicted_class<-ifelse(predict(prod_sales_Logit_model,type="response")>threshold,1,0)
cat("Predcited Classes")
predicted_class[1:10]

actual_values<-prod_sales_Logit_model$y
conf_matrix<-table(predicted_class,actual_values)
cat("Confusion Matrix")
conf_matrix


accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
cat("Accuracy")
accuracy


##############Multicollinearity 
cat("Need car package")
library(car)

summary(Fiberbits_model_1)

cat("use VIF for identifying the Multicollinearity")
vif(Fiberbits_model_1)



##############Individual Impact of Variables
library(caret)
summary(Fiberbits_model_1)
varImp(Fiberbits_model_1, scale = FALSE)



#######################################################
###############Model Selection

cat("Model Selection")
library(caret)

##Accuracy of model1
threshold=0.5
predicted_values<-ifelse(predict(Fiberbits_model_1,type="response")>threshold,1,0)
actual_values<-Fiberbits_model_1$y
conf_matrix<-table(predicted_values,actual_values)
accuracy1<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy1


##################AIC and BIC of Model1
library(stats)
AIC(Fiberbits_model_1)
BIC(Fiberbits_model_1)

summary(Fiberbits_model_1)
varImp(Fiberbits_model_1, scale = FALSE)


#Income is the least impacting variable, lets drop it and re build the model
Fiberbits_model_2<-glm(active_cust~months_on_network+Num_complaints+number_plan_changes+relocated+monthly_bill+technical_issues_per_month+Speed_test_result,family=binomial(),data=Fiberbits)
summary(Fiberbits_model_2)

#Accuracy of model2
threshold=0.5
predicted_values<-ifelse(predict(Fiberbits_model_2,type="response")>threshold,1,0)
actual_values<-Fiberbits_model_2$y
conf_matrix<-table(predicted_values,actual_values)
accuracy2<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy2


##################AIC and BIC of Model2

AIC(Fiberbits_model_1,Fiberbits_model_2)
BIC(Fiberbits_model_1,Fiberbits_model_2)

#####Dropping Income has not reduced the accuracy. We will leave it

#Adding more variables to increase the accuracy
Fiberbits_model_3<-glm(active_cust~
					  income
					  +months_on_network
					  +Num_complaints
					  +number_plan_changes
					  +relocated
					  +monthly_bill
					  +technical_issues_per_month
					  +technical_issues_per_month*number_plan_changes
					  +Speed_test_result+I(Speed_test_result^2),
					  family=binomial(),data=Fiberbits)
summary(Fiberbits_model_3)

#Accuracy of model3
threshold=0.5
predicted_values<-ifelse(predict(Fiberbits_model_3,type="response")>threshold,1,0)
actual_values<-Fiberbits_model_3$y
conf_matrix<-table(predicted_values,actual_values)
accuracy3<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy3


##################AIC and BIC of Model3
AIC(Fiberbits_model_3)
BIC(Fiberbits_model_3)



#Comparison of All three models
#Comparing Accuracy
c(accuracy1,accuracy2,accuracy3)

#Comparing AIC and BIC
AIC(Fiberbits_model_1,Fiberbits_model_2,Fiberbits_model_3)
BIC(Fiberbits_model_1,Fiberbits_model_2,Fiberbits_model_3)













