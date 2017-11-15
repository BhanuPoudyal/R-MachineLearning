

#Importing Air passengers data
air <- read.csv("AirPassengers.csv")
dim(air)
names(air)
head(air)

#Find the correlation between number of passengers and promotional budget.
cor(air$Passengers, air$Promotion_Budget)

#Find the correlation between number of passengers and Inter_metro_flight_ratio.
cor(air$Passengers, air$Inter_metro_flight_ratio)


##############################################################
################Regression##################################
############################################################

#Correlation between promotion and passengers count
cor(air$Promotion_Budget,air$Passengers)

#Scatter Plot between  promotion and passengers count
plot(air$Promotion_Budget,air$Passengers)

#Regression Model  promotion and passengers count
model1<-lm(Passengers~Promotion_Budget, data=air)
summary(model1)

#Potting the Regression line
plot(air$Promotion_Budget,air$Passengers,col = "blue", xlab)
abline(model1, lwd = 5, col="red")

#Prediction 
newdata = data.frame(Promotion_Budget=650000)
predict(model1, newdata)

#Prediction 
newdata = data.frame(Promotion_Budget=517356)
predict(model1, newdata)

#Prediction 
newdata = data.frame(Promotion_Budget=700000)
predict(model1, newdata)

#Regression Model inter_metro_flight_ratio and passengers count
plot(air$Inter_metro_flight_ratio,air$Passengers)
model2<-lm(Passengers~Inter_metro_flight_ratio, data=air)
summary(model2)

################################################
#Multiple Regerssion Model
multi_model<-lm(Passengers~Promotion_Budget+Inter_metro_flight_ratio+Service_Quality_Score , data=air)
summary(multi_model)

################################################
#Imapct of Variables
#Dropped Inter_metro_flight_ratio variable due to high p-value
multi_model_1<-lm(Passengers~Promotion_Budget+Service_Quality_Score , data=air)
summary(multi_model_1)

#Dropped Promotion_Budget 
multi_model_2<-lm(Passengers~Service_Quality_Score , data=air)
summary(multi_model_2)

###############################################
##Adjusted R-Square
adj_sample<-read.csv("Adj_Sample.csv")

m1<-lm(Y~x1+x2+x3,data=adj_sample)
summary(m1)

m2<-lm(Y~x1+x2+x3+x4+x5+x6, data=adj_sample)
summary(m2)

m3<-lm(Y~x1+x2+x3+x4+x5+x6+x7+x8, data=adj_sample)
summary(m3)


################################################
#####Multiple Regression- issues
final_exam<- read.csv("Final Exam Score.csv")

cat("The size of data")
dim(final_exam)

cat("The names of the variables")
names(final_exam)

cat("First few observations")
head(final_exam)

exam_model<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem1_Math+Sem2_Math, data=final_exam)
summary(exam_model)

#After dropping Sem1_Math
exam_model2<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem2_Math, data=final_exam)
summary(exam_model2)

#Sctter Plot between the predictor variables
plot(final_exam$Sem1_Math,final_exam$Sem2_Math)
cor(final_exam$Sem1_Math,final_exam$Sem2_Math)

###############################################
##Multicollinearity detection
##Testing Multicollinearity
###Need car package (Companion to Applied Regression)
library(car)
exam_model<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem1_Math+Sem2_Math, data=final_exam)
summary(exam_model)
vif(exam_model)

exam_model1<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem2_Math, data=final_exam)
summary(exam_model1)
vif(exam_model1)

exam_model2<-lm(Final_exam_marks~Sem2_Science+Sem2_Math, data=final_exam)
summary(exam_model2)

cat("VIF Values of each variable")
vif(exam_model2)


###############################################
##Multicollinearity 


Webpage_Product_Sales<-read.csv("D:\\Google Drive\\Training\\Datasets\\Webpage_Product_Sales\\Webpage_Product_Sales.csv")

dim(Webpage_Product_Sales)
names(Webpage_Product_Sales)


web_sales_model<-lm(Sales~Web_UI_Score+Server_Down_time_Sec+Holiday+Special_Discount+Clicks_From_Serach_Engine+Online_Ad_Paid_ref_links+Social_Network_Ref_links+Month+Weekday+DayofMonth, data=Webpage_Product_Sales)
summary(web_sales_model)


###Checking for VIF
vif(web_sales_model)


##Dropped Clicks_From_Serach_Engine based on VIF
web_sales_model1<-lm(Sales~Web_UI_Score+Server_Down_time_Sec+Holiday+Special_Discount+Clicks_From_Serach_Engine+Social_Network_Ref_links+Month+Weekday+DayofMonth, data=Webpage_Product_Sales)
summary(web_sales_model1)


##Dropped Web_UI_Score based on P-value
web_sales_model2<-lm(Sales~Server_Down_time_Sec+Holiday+Special_Discount+Clicks_From_Serach_Engine+Social_Network_Ref_links+Month+Weekday+DayofMonth, data=Webpage_Product_Sales)
summary(web_sales_model2)



###############################################
#####Adding Interaction Terms
web_sales_model_3<-lm(Sales~Server_Down_time_Sec+Holiday+Special_Discount+Online_Ad_Paid_ref_links+Social_Network_Ref_links+Month+Weekday+DayofMonth+Holiday*Weekday,data=Webpage_Product_Sales)
summary(web_sales_model_3)





