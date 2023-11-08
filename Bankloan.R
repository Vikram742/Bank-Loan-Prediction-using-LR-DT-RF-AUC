#read the data
bank<-read.csv("bankloan.csv")
#### DATA CLEANING
#check the data types
str(bank)
#need to change data types of certain columns
bank$CreditCard<-as.factor(bank$CreditCard)
bank$Online<-as.factor(bank$Online)
bank$CD.Account<-as.factor(bank$CD.Account)
bank$Securities.Account<-as.factor(bank$Securities.Account)
bank$Personal.Loan<-as.factor(bank$Personal.Loan)
bank$Education<-as.factor(bank$Education)
#check for missing values
sum(is.na(bank))
#no missing values found
#check for duplicates
sum(duplicated(bank))
#no duplicate records found
# I don't know what does the column experience imply. So I am dropping the column along with ID column.

bank1<- bank[,c(2,4:14)] 
  
##### EXPLORATORY DATA ANALYSIS
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
bank1 %>% 
  group_by(Personal.Loan) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x = Personal.Loan, y = n))+
  geom_bar(stat = 'identity', fill = 'purple')+
  geom_label(aes(label = n))+
  theme_minimal()+
  labs(title = 'Count of loans approved and not approved', x = 'Loan Status', y = 'Count')+
  theme(panel.grid = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
  #Out of the total 5000 customers, 4520 have not been approved for a loan while 480 have been

#To understand the impact of variables like Age & Income, it is better to put them in a range or category
bank1$Age_range<-cut(bank1$Age, breaks = c(22,30,40,50,60,70)
                     ,labels = c("22-30","31-40","41-50","51-60","61-70"))
table(bank1$Income)
bank1$Income_class<-cut(bank1$Income, breaks = c(7,24,100,150,200,230),
                        labels = c('Poor','MiddleClass','UpperMiddleClass','Rich','SuperRich'))  



  
#### BOXPLOTS
#INCOME AND FAMILY  
  bank1 %>% 
    ggplot(aes(x = Family, y = Income, color = as.factor(Family)))+
    geom_boxplot()
#THIS INDICATES THAT INCOME IS HIGHER WHEN THERE ARE LESS FAMILY MEMBERS
  
#INCOME AND PERSONAL LOAN
  bank1 %>% 
    ggplot(aes(x = Personal.Loan, y = Income, color = Personal.Loan))+
    geom_boxplot()
#THIS INDICATES PERSONAL LOAN HAS BEEN APPROVED FOR CUSTOMERS HAVING HIGHER INCOME
  
#INCOME AND CREDIT CARDS
  bank1 %>% 
    ggplot(aes(x = CreditCard, y = Income, color = CreditCard))+
    geom_boxplot()
#THIS INDICATES THAT THE INCOME IS PRETTY SIMILAR FOR CUSTOMERS OWNING AND NOT OWNING A CREDIT CARD   
  
#INCOME CLASS AND MORTGAGE 
  bank1 %>% 
    ggplot(aes(x = Income_class, y = Mortgage, color = Income_class))+
    geom_boxplot()
#CUSTOMERS BELONGING TO THE RICH CLASS (INCOME GROUP : 150-200) HAVE THE HIGHEST MORTGAGE  
  
# CC AVG AND ONLINE
  bank1 %>% 
    ggplot(aes(x = Online, y = CCAvg, color = Online))+
    geom_boxplot()
  
#CC AVG IS PRETTY SIMILAR FOR THOSE WHO OPTED FOR ONLINE SERVICES AND THOSE WHO DID NOT  
  
# CC AVG AND EDUCATION
  bank1 %>% 
    ggplot(aes(x = Education, y = CCAvg, color = Education))+
    geom_boxplot()
# MORE EDUCATED HAVE A HIGHER CREDIT AVERAGE
  
#CC AVG and AGE RANGE
  bank1 %>% 
    ggplot(aes(x = Age_range, y = CCAvg, color = Age_range))+
    geom_boxplot()

##### LOGISTIC REGRESSION
model<- glm(Personal.Loan~.,data = bank1, family = 'binomial')  
summary(model)

#try to remove insignificant variables
model1<-step(model,direction='backward',trace=0)
summary(model1)
# Zipcode variable has been removed 

bank2<- bank1[,c(1,2,4:14)]

#check for multicollinearity
library(car)
vif(model1)
# Age, Income and Age_range have a VIF value greater than 5. So we will drop the Age_range first.

bank3<-bank2[,c(1:11,13)]

model2<-glm(Personal.Loan~., data=bank3, family = 'binomial')
summary(model2)
# We will run the vif once again to check the values. 
vif(model2)

#Split the data into train and test
library(caret)
set.seed(1234)
index<-createDataPartition(bank3$Personal.Loan, p = 0.8, list = FALSE )
train<-bank3[index,]
test<-bank3[-index,]

#We use the train data to create the model and also use the step function
model3<- glm(Personal.Loan~., data = train, family = 'binomial')
summary(model3)

model4<-step(model3, direction = 'backward', trace = 0)
summary(model4)
# Column 'Mortgage' has been removed.



#predict the test data
predicted<-predict(model4, newdata = test, type = 'response')
predicted
test$predicted<-predicted
test$class<-ifelse(test$predicted>=0.5,1,0)
str(test$class)
#It is a numerical vector which needs to be converted to factor vector
test$class<-as.factor(test$class)
str(test$class)
#It has been converted to a factor vector

#Confusion Matrix
confusionMatrix(test$class,test$Personal.Loan,positive = "1")
#Accuracy is 96.10%, Sensitivity is 70.83% and Specificity is 98.78%

##AS WE SAW EARLIER THAT THE DATA IS HEAVILY IMBALANCED
##Out of the total 5000 customers, 4520 have not been approved for a loan while 480 have been
## WE NEED TO BALANCE THE DATA

library(ROSE)
table(train$Personal.Loan)
#3616 customers have not been approved for a loan while 384 has been approved
#over data = 3616+3616 = 7232
#under data = 384+384 = 768
#both data = 3616+384 = 4000

set.seed(1234)
over_data<- ovun.sample(Personal.Loan~., data = train, method = 'over', N = 7232)$data
table(over_data$Personal.Loan)

set.seed(1234)
under_data<-ovun.sample(Personal.Loan~., data = train, method = 'under', N = 768)$data
table(under_data$Personal.Loan)

set.seed(1234)
both_data<-ovun.sample(Personal.Loan~., data = train, method = 'both', p = 0.5, N = 4000)$data
table(both_data$Personal.Loan)

#Predict the test data for over, under and both data using Logistic Regression
#over_data - LR
set.seed(1234)
lr_over_model<-glm(Personal.Loan~., data = over_data, family = 'binomial')
predict_lr_over<-predict(lr_over_model, newdata = test, type = 'response')
predict_lr_over
test$predict_lr_over<-predict_lr_over
test$class_lr_over<- ifelse(test$predict_lr_over >=0.5,1,0)
str(test$class_lr_over)
test$class_lr_over<-as.factor(test$class_lr_over)
confusionMatrix(test$class_lr_over,test$Personal.Loan,positive = "1")
#Accuracy is 92.1%, Sensitivity is 94.79% and Specificity is 91.81% - LR for over_data

#under_data - LR
set.seed(1234)
lr_under_model<-glm(Personal.Loan~., data = under_data, family = 'binomial')
predict_lr_under<- predict(lr_under_model, newdata = test, type = 'response')
predict_lr_under
test$predict_lr_under<-predict_lr_under
test$class_lr_under<- ifelse(test$predict_lr_under >=0.5 , 1, 0)
str(test$class_lr_under)
test$class_lr_under<-as.factor(test$class_lr_under)
confusionMatrix(test$class_lr_under,test$Personal.Loan, positive = '1')
#Accuracy is 92.1%, Sensitivity is 95.83% and Specificity is 91.70% - LR for under_data

#both_data - LR
set.seed(1234)
lr_both_model<- glm(Personal.Loan~., data = both_data, family = 'binomial')
predict_lr_both<-predict(lr_both_model, newdata = test, type = 'response')
predict_lr_both
test$predict_lr_both<-predict_lr_both
test$class_lr_both<- ifelse(test$predict_lr_both >= 0.5,1,0)
str(test$class_lr_both)
test$class_lr_both<-as.factor(test$class_lr_both)
confusionMatrix(test$class_lr_both,test$Personal.Loan, positive = '1')
#Accuracy is 92.2%, Sensitivity is 93.75% and Specificity is 92.04% - LR for both_data


################DECISION TREE
library(rpart)
# over_data - DT
set.seed(1234)
dt_over_model<-rpart(Personal.Loan~., data = over_data)
predict_dt_over<-predict(dt_over_model, newdata = test, type = 'class')
predict_dt_over
test$predict_dt_over<-predict_dt_over
str(test$predict_dt_over)
confusionMatrix(test$predict_dt_over,test$Personal.Loan,positive = "1")
#Accuracy is 92.80%, Sensitivity is 98.96% and Specificity is 92.15% - DT for over_data

# under_data - DT
set.seed(1234)
dt_under_model<-rpart(Personal.Loan~., data = under_data)
predict_dt_under<-predict(dt_under_model,newdata = test, type = 'class')
predict_dt_under
test$predict_dt_under<-predict_dt_under
str(test$predict_dt_under)
confusionMatrix(test$predict_dt_under,test$Personal.Loan,positive = "1")
#Accuracy is 93.70%, Sensitivity is 98.96% and Specificity is 93.14% - DT for under_data

# both_data - DT
set.seed(1234)
dt_both_model<-rpart(Personal.Loan~., data = both_data)
predict_dt_both<-predict(dt_both_model,newdata = test, type = 'class')
predict_dt_both
test$predict_dt_both<-predict_dt_both
str(test$predict_dt_both)
confusionMatrix(test$predict_dt_both,test$Personal.Loan, positive = "1")
#Accuracy is 94.50%, Sensitivity is 94.79% and Specificity is 94.47% - DT for both_data


##################RANDOM FOREST
library(randomForest)
# over_data - RF
set.seed(1234)
rf_over_model<-randomForest(Personal.Loan~., data= over_data)
predict_rf_over<- predict(rf_over_model, newdata = test, type = 'class')
test$predict_rf_over<-predict_rf_over
str(test$predict_rf_over)
confusionMatrix(test$predict_rf_over,test$Personal.Loan,positive = "1")
#Accuracy is 98.50%, Sensitivity is 92.71% and Specificity is 99.12% - RF for over_data

# under_data - RF
set.seed(1234)
rf_under_model<-randomForest(Personal.Loan~., data = under_data)
predict_rf_under<- predict(rf_under_model,newdata = test, type = 'class')
test$predict_rf_under<-predict_rf_under
str(test$predict_rf_under)
confusionMatrix(test$predict_rf_under,test$Personal.Loan,positive = "1")
#Accuracy is 95.70, Sensitivity is 98.96% and Specificity is 95.35% - RF for under_data

# both_data - RF
set.seed(1234)
rf_both_model<- randomForest(Personal.Loan~., data = both_data)
predict_rf_both<- predict(rf_both_model,newdata=test,type = 'class')
test$predict_rf_both<-predict_rf_both
str(test$predict_rf_both)
confusionMatrix(test$predict_rf_both,test$Personal.Loan,positive = "1")
#Accuracy is 98.40%, Sensitivity is 97.92% and Specificity is 98.45% - RF for both_data


######ROC and AUC
#over_data - LR
library(pROC)
set.seed(1234)
pred_lr_over<-predict(lr_over_model,newdata = test, type = "response")
pred_lr_over
test$pred_lr_over<-pred_lr_over
par(pty ="s")
roc(test$Personal.Loan,test$pred_lr_over,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = "#8BC3BE",
    lwd=1, print.auc=TRUE, main = 'AUC FOR LOGISTIC REGRESSION FOR OVER BALANCED DATA')
#AUC 98.01% for over_data for Logistic Regression

#under_data - LR
set.seed(1234)
pred_lr_under<- predict(lr_under_model,newdata = test, type = 'response')
pred_lr_under
test$pred_lr_under<-pred_lr_under
par(pty="s")
roc(test$Personal.Loan,test$pred_lr_under,plot=TRUE,percent=TRUE,legacy.axes =TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col = "#BC8BC3",
    lwd = 1, print.auc =TRUE, main = 'AUC FOR LOGISTIC REGRESSION FOR UNDER BALANCED DATA')
#AUC 98.2% for under_data for Logistic Regression

#both_data - LR
set.seed(1234)
pred_lr_both<-predict(lr_both_model, newdata = test, type ='response')
pred_lr_both
test$pred_lr_both<-pred_lr_both
par(pty="s")
roc(test$Personal.Loan,test$pred_lr_both,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col = "#BFC38B",
    lwd = 1, print.auc=TRUE, main = 'AUC FOR LOGISTIC REGRESSION FOR BOTH BALANCED DATA')
#AUC 97.86% for both_data for Logistic Regression

#over_data - DT
set.seed(1234)
pred_dt_over<-predict(dt_over_model, newdata=test, type = 'prob')
pred_dt_over
test$pred_dt_over<-pred_dt_over[,2]
par(pty="s")
roc(test$Personal.Loan,test$pred_dt_over,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col ="#4daf4a",
    lwd = 1, print.auc = TRUE, main = 'AUC FOR DECISION TREE FOR OVER BALANCED DATA')
#AUC 97.72% for over_data for Decision Tree

#under_data - DT
set.seed(1234)
pred_dt_under<-predict(dt_under_model,newdata = test,type = 'prob')
pred_dt_under
test$pred_dt_under<-pred_dt_under[,2]
par(pty = 's')
roc(test$Personal.Loan,test$pred_dt_under,plot=TRUE,percent=TRUE,legacy.axes = TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col ="#D40D52",
    lwd=1,print.auc=TRUE, main = 'AUC FOR DECISION TREE FOR UNDER BALANCED DATA' )
#AUC 98.01% for under_data for Decision Tree

#both_data - DT
set.seed(1234)
pred_dt_both<-predict(dt_both_model,newdata = test, type = 'prob')
pred_dt_both
test$pred_dt_both<- pred_dt_both[,2]
par(pty='s')
roc(test$Personal.Loan,test$pred_dt_both,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col="#377eb8",
    lwd = 1, print.auc=TRUE, main = 'AUC FOR DECISION TREE FOR BOTH BALANCED DATA')
#AUC 98.80% for both_data for Decision Tree

#over_data - RF
set.seed(1234)
pred_rf_over<- predict(rf_over_model,newdata = test,type ='prob')
pred_rf_over
test$pred_rf_over<-pred_rf_over[,2]
par(pty ='s')
roc(test$Personal.Loan,test$pred_rf_over,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col = "#C38BBD",
    lwd = 1, print.auc=TRUE, main = 'AUC FOR RANDOM FOREST FOR OVER BALANCED DATA')
#AUC 99.83% for over_data for Random Forest

#under_data - RF
set.seed(1234)
pred_rf_under<- predict(rf_under_model,newdata = test, type = 'prob')
pred_rf_under
test$pred_rf_under<-pred_rf_under[,2]
par(pty ='s')
roc(test$Personal.Loan,test$pred_rf_under,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col="#C2C38B",
    lwd = 1, print.auc=TRUE, main = 'AUC FOR RANDOM FOREST FOR UNDER BALANCED DATA')
#AUC 99.71% for under_data for Random Forest

#both_data - RF
set.seed(1234)
pred_rf_both<-predict(rf_both_model,newdata = test, type = 'prob')
pred_rf_both
test$pred_rf_both<-pred_rf_both[,2]
par(pty ='s')
roc(test$Personal.Loan,test$pred_rf_both,PLOT=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab = 'FALSE POSITIVE PERCENTAGE', ylab = 'TRUE POSITIVE PERCENTAGE', col="#DFFF00",
    lwd = 1, print.auc=TRUE, main ='AUC FOR RANDOM FOREST FOR BOTH BALANCED DATA')
#AUC 99.82% for both_data for Random Forest

## CONCLUSION: IF WE DECIDE TO GO WITH AUC , THEN WE CAN MOVE AHEAD WITH RANDOM FOREST

