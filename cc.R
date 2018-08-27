
# Data Importing

Final_Data_prep <- read.csv("Path")
View(Final_Data_prep)


mydata <- Final_Data_prep

# Data Summary
str(mydata, list.len  = 150)


# Col names

colnames(mydata)

# [1] "Type"                               "Associate.ID"                      
# [3] "Job.Title_CS_Service_Specialist"    "Job.Title_IND_Team_Developer_WOA"  
# [5] "Job.Title_IND_Team_Developer_WOA.1" "Aon.Pay.Level"                     
# [7] "Location.Group"                     "BU_HRBPO"                          
# [9] "BU_AM"                              "BU_YSA"                            
# [11] "BU_CANADA"                          "TenurePayBand_1_2"                 
# [13] "TenurePayBand_2_3"                  "TenurePayBand_3_4"                 
# [15] "TenurePayBand_4."                   "TenureJobCode_1_2"                 
# [17] "TenureJobCode_2_3"                  "TenureJobCode_3_4"                 
# [19] "TenureJobCode_4."                   "Gender"                            
# [21] "RT_Voice"                           "RT_WebCHat"                        
# [23] "TenureinAon_1_2"                    "TenureinAON_2_3"                   
# [25] "TenureinAON_3_4"                    "TenureinAON_4."                    
# [27] "Age"                                "Engagement.Score" 


#Data Exploration and Data Understanding using Descriptive Analytics

install.packages("psych")
library(psych)
install.packages("ISLR")
library(ISLR) 
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)

describe(mydata$Age)

# Converating data into factors

mydata$Job.Title_CS_Service_Specialist <- factor(mydata$Job.Title_CS_Service_Specialist)
mydata$Job.Title_IND_Team_Developer_WOA <- factor(mydata$Job.Title_IND_Team_Developer_WOA)
mydata$Job.Title_IND_Team_Developer_WOA.1 <- factor(mydata$Job.Title_IND_Team_Developer_WOA.1)

mydata$Aon.Pay.Level <- factor(mydata$Aon.Pay.Level)

mydata$Location.Group <- factor(mydata$Location.Group)

mydata$Gender <- factor(mydata$Gender)

mydata$BU_HRBPO <- factor(mydata$BU_HRBPO)
mydata$BU_AM <- factor(mydata$BU_AM)
mydata$BU_YSA <- factor(mydata$BU_YSA)
mydata$BU_CANADA <- factor(mydata$BU_CANADA)


mydata$TenurePayBand_1_2 <- factor(mydata$TenurePayBand_1_2)
mydata$TenurePayBand_2_3 <- factor(mydata$TenurePayBand_2_3)
mydata$TenurePayBand_3_4 <- factor(mydata$TenurePayBand_3_4)
mydata$TenurePayBand_4. <- factor(mydata$TenurePayBand_4.)

mydata$TenureJobCode_1_2 <- factor(mydata$TenureJobCode_1_2)
mydata$TenureJobCode_2_3 <- factor(mydata$TenurePayBand_2_3)
mydata$TenureJobCode_3_4 <- factor(mydata$TenureJobCode_3_4)
mydata$TenureJobCode_4. <- factor(mydata$TenureJobCode_4.)

mydata$RT_Voice <- factor(mydata$RT_Voice)
mydata$RT_WebCHat <- factor(mydata$RT_WebCHat)

mydata$TenureinAon_1_2 <- factor(mydata$TenureinAon_1_2)
mydata$TenureinAON_2_3 <- factor(mydata$TenureinAON_2_3)
mydata$TenureinAON_3_4 <- factor(mydata$TenureinAON_3_4)
mydata$TenureinAON_4. <- factor(mydata$TenureinAON_4.)

str(mydata, list.len  = 150)



# Randomly choosing development sample with Non-Attrited(Good - Type 0) and Attrited (Bad - Type 1)

set.seed(123)
library(caTools)
train_ind <- sample(1:nrow(mydata), size = floor(0.70 * nrow(mydata)))

Development<-mydata[train_ind,]
Validation <-mydata[-train_ind,]

table(mydata$Type)
table(Development$Type)
str(Development)
table(Validation$Type)
str(Validation)

# Different fits for the robust models

fit <- step(glm(formula = Type ~ Job.Title_CS_Service_Specialist + Job.Title_IND_Team_Developer_WOA
                + Job.Title_IND_Team_Developer_WOA.1 + Aon.Pay.Level + Location.Group + BU_HRBPO
                + BU_AM + BU_YSA + BU_CANADA + TenurePayBand_1_2 + TenurePayBand_2_3 + TenurePayBand_3_4
                + TenurePayBand_4. + TenureJobCode_1_2 + TenureJobCode_2_3 + TenureJobCode_3_4 + TenureJobCode_4.
                + TenureJobCode_4. + Gender + RT_Voice + RT_WebCHat + TenureinAon_1_2 + TenureinAON_2_3 + TenureinAON_3_4
                + Age + Engagement.Score, family=binomial,data = Development))
summary(fit)


fit_a <- step(glm(formula = Type ~ Job.Title_CS_Service_Specialist + Job.Title_IND_Team_Developer_WOA
                + Job.Title_IND_Team_Developer_WOA.1 + Aon.Pay.Level + Location.Group + BU_HRBPO
                + BU_AM + BU_YSA + BU_CANADA + Gender + RT_Voice + RT_WebCHat + TenureinAon_1_2 + TenureinAON_2_3 + TenureinAON_3_4
                + Age + Engagement.Score, family=binomial,data = Development))
summary(fit_a)


## Best fit

fit_1 <- glm(formula =  Type ~ Job.Title_CS_Service_Specialist + Job.Title_IND_Team_Developer_WOA + 
               Job.Title_IND_Team_Developer_WOA.1 + BU_CANADA + TenurePayBand_2_3 + 
               TenurePayBand_4. + RT_Voice + RT_WebCHat + TenureinAon_1_2 + 
               TenureinAON_2_3, family=binomial,data = Development)

summary(fit_1)

## Best fit from fit a

fit_1 <- glm(formula = Type ~ Job.Title_CS_Service_Specialist + Job.Title_IND_Team_Developer_WOA + 
               Job.Title_IND_Team_Developer_WOA.1 + BU_CANADA + RT_Voice + 
               RT_WebCHat + TenureinAon_1_2 + Age + Engagement.Score , 
             family=binomial,data = Development)

summary (fit_1)


## fit

fit_1 <- glm(formula =  Type ~ Job.Title_CS_Service_Specialist + Job.Title_IND_Team_Developer_WOA
             + Job.Title_IND_Team_Developer_WOA.1 +   BU_HRBPO
             + BU_AM + BU_YSA + BU_CANADA + TenurePayBand_1_2 + TenurePayBand_2_3 + TenurePayBand_3_4
             + TenurePayBand_4. + Gender + RT_Voice + RT_WebCHat + Age + Engagement.Score, family=binomial,data = Development)

summary(fit_1)

# Picking fit_1 for the time being

summary(fit_1)
ls(fit_1)
model_fit<-fit_1$model
coeff<-fit_1$coef 
write.csv(coeff, "CE_Fit1.csv")


# Anova 

anova(fit_1, test = 'Chisq')


# Wald Test

install.packages("aod")
library(aod)

wald.test(b= coef(fit_1), Sigma = vcov(fit_1), Terms = 4)

# Mc Fadden  R^2 index

install.packages('pscl')
library(pscl)
pR2(fit_1)


#Prediction on Development

install.packages("e1071")
library(e1071)
install.packages("verification")
library(verification)
pred <- predict(fit_1,newdata=Development,type="response")
auc <- roc.area(Development$Type, pred)
auc


#Model Accuracy Measures and Area under the curve 

install.packages("ROCR")

library(ROCR)

lr_fitpred <- prediction(pred,Development$HC.ATT)
roc <- performance(lr_fitpred,"tpr","fpr")
plot(roc,col="blue",lwd=3,main="ROC Curve for Logistic Regression:BEST AIC")
abline(a=0,b=1,lwd=3,lty=2,col="red")


#create data frame of values 

perf <-as.data.frame(cbind(roc@alpha.values[[1]], 
                           roc@x.values[[1]], 
                           roc@y.values[[1]])) 
colnames(perf) <-c("Probability","FPR","TPR") 


perf <-perf[-1,] 


#reshape the data frame 
install.packages("reshape")
library(reshape) 
perf2<- melt(perf, measure.vars = c("FPR", "TPR")) 


#Plotting FPR, TPR on y axis and cut-off probability on x axis 

ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()


#model accuracy - Confusion Matrix

install.packages("SDMTools")
library(SDMTools) 
confusion.matrix (Development$Type, pred, threshold = 0.50)

#Concordance 

source(choose.files())
Concordance(fit_1)


#odds ratios and 95% CI 

exp(cbind(OR = coef(fit_1), confint(fit_1))) 

#Multicolleniarity Check

install.packages('car')
library(car)

exp(cbind(OR = coef(fit_1) ,confint(fit_1))) 

vif(fit_1)

#In real life, when we deal with high number of observations, often experts urge that it is not of a major issue if multicollinearity is not completelt removed. 
#This is because as the number of observations go high, in lakh or million, impact of multicollinearity comes down.


#Autocorrelation Check

durbinWatsonTest(fit_1)



#_____________________________Prediction on Validation____________________________________# 


fit_test <- glm(formula =  Type ~ Aon.Pay.Level + Location.Group + BU + Resource.Type + Tenure.in.AON + Age.Bins + Engagement.Bins,data = 
                  Validation)
summary(fit_test)

install.packages("e1071")
library(e1071)
install.packages("verification")
library(verification)
pred <- predict(fit_4,newdata=Validation,type="response")
auc <- roc.area(Validation$Type, pred)
auc

#Model Accuracy Measures and Area under the curve 
library(ROCR)
lr_fitpred <- prediction(pred,Validation$Type)
roc <- performance(lr_fitpred,"tpr","fpr")
plot(roc,col="blue",lwd=3,main="ROC Curve for Logistic Regression:BEST AIC")
abline(a=0,b=1,lwd=3,lty=2,col="red")


#create data frame of values 


perf <-as.data.frame(cbind(roc@alpha.values[[1]], 
                           roc@x.values[[1]], 
                           roc@y.values[[1]])) 
colnames(perf) <-c("Probability","FPR","TPR") 


#removing infinity value from data frame 

perf <-perf[-1,] 


#reshape the data frame 

install.packages("reshape")
library(reshape) 
perf2<- melt(perf, measure.vars = c("FPR", "TPR")) 


#Plotting FPR, TPR on y axis and cut-off probability on x axis 

library('ggplot2')

ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()



#model accuracy - Confusion Matrix

install.packages("SDMTools")
library(SDMTools)
confusion.matrix (Validation$Type, pred, threshold = 0.50)

source(choose.files())
Concordance(fit_test)


#Decile Scoring for Development dataset

train1<- cbind(Development, Prob=predict(fit_4, type="response")) 
View(train1)


#Creating Deciles

decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
train1$decile<-factor(train1$decile)


#Decile Analysis Reports

install.packages("sqldf")
library(sqldf)
fit_train_DA <- sqldf("select decile, min(Prob) as Min_prob
                      , max(Prob) as max_prob
                      , sum(Type) as Attrited_Count
                      , (count(decile)-sum(Type)) as Non_Attrited_Count 
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_4_Train_CE.csv",row.names = F)

#Decile Scoring for validation dataset

test1<- cbind(Validation, Prob=predict(fit_3,Validation, type="response")) 
View(test1)
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
test1$decile<-factor(test1$decile)
names(test1)

write.csv(test1,"Fit4_Test_CE.csv",row.names = F)

#Decile Analysis Reports


fit_test_DA <- sqldf("select decile, min(Prob) as Min_prob
                     , max(Prob) as max_prob
                     , sum(Type) as Attrited_Count
                     , (count(decile)-sum(Type)) as Non_Attrited_Count 
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_CE.csv",row.names = F)

#Prediction for Att for Validation data set

New_Employee<-cbind(Validation, Prob=predict(fit_4, newdata=Validation, type="response"))
View(New_Employee)
New_Employee$Type_New <- ifelse(New_Employee$Prob>0.50, 1,0)
sum(New_Employee$Type_New)


#Customer Profiling using Deciles

Employee_Profile<-data.frame(rbind(train1,test1))
write.csv(Employee_Profile,"CE_EMployee_profile.csv",row.names = F)                              


#You can also exponentiate the coefficients and interpret them as odds-ratios.

# odds ratios only

exp(coef(fit_4))


## odds ratios and 95% CI
odds <-exp(cbind(OR = coef(fit_4), confint(fit_4)))

write.csv(odds, "odds.csv")


#_________*********** Picking fit_5 ***********__________________#

summary(fit_5)
ls(fit_5)
model_fit<-fit_5$model
coeff<-fit_5$coef 
write.csv(coeff, "CE_Fit5.csv")


# Anova 

anova(fit_5, test = 'Chisq')


# Wald Test

install.packages("aod")
library(aod)

wald.test(b= coef(fit_5), Sigma = vcov(fit_5), Terms = 4)

# Mc Fadden  R^2 index

install.packages('pscl')
library(pscl)
pR2(fit_5)


#Prediction on Development

install.packages("e1071")
library(e1071)
install.packages("verification")
library(verification)
pred <- predict(fit_5,newdata=Development,type="response")
auc <- roc.area(Development$Type, pred)
auc


#Model Accuracy Measures and Area under the curve 

install.packages("ROCR")

library(ROCR)

lr_fitpred <- prediction(pred,Development$Type)
roc <- performance(lr_fitpred,"tpr","fpr")
plot(roc,col="blue",lwd=3,main="ROC Curve for Logistic Regression:BEST AIC")
abline(a=0,b=1,lwd=3,lty=2,col="red")


#create data frame of values 

perf <-as.data.frame(cbind(roc@alpha.values[[1]], 
                           roc@x.values[[1]], 
                           roc@y.values[[1]])) 
colnames(perf) <-c("Probability","FPR","TPR") 


perf <-perf[-1,] 


#reshape the data frame 
install.packages("reshape")
library(reshape) 
perf2<- melt(perf, measure.vars = c("FPR", "TPR")) 


#Plotting FPR, TPR on y axis and cut-off probability on x axis 
library(ggplot2)
ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()


#model accuracy - Confusion Matrix

install.packages("SDMTools")
library(SDMTools) 
confusion.matrix (Development$Type, pred, threshold = 0.70)

#Concordance 

source(choose.files())
Concordance(fit_5)


#odds ratios and 95% CI 

exp(cbind(OR = coef(fit_5), confint(fit_5))) 

#Multicolleniarity Check

install.packages('car')
library(car)

exp(cbind(OR = coef(fit_5) ,confint(fit_5))) 

vif(fit_5)

#In real life, when we deal with high number of observations, often experts urge that it is not of a major issue if multicollinearity is not completelt removed. 
#This is because as the number of observations go high, in lakh or million, impact of multicollinearity comes down.


#Autocorrelation Check

durbinWatsonTest(fit_5)



#_____________________________Prediction on Validation____________________________________# 

fit_5 <- glm(formula = Type ~ Aon.Pay.Level + Location.Group + BU + Resource.Type + Tenure.In.Pay.Band + Age.Bins + Engagement.Bins,                  
             family=binomial,data = Development)

fit_test <- glm(formula =  Type ~ Aon.Pay.Level + Location.Group + BU + Resource.Type + Tenure.In.Pay.Band + Age.Bins + Engagement.Bins,data = 
                  Validation)
summary(fit_test)

install.packages("e1071")
library(e1071)
install.packages("verification")
library(verification)
pred <- predict(fit_5,newdata=Validation,type="response")
auc <- roc.area(Validation$Type, pred)
auc

#Model Accuracy Measures and Area under the curve 
library(ROCR)
lr_fitpred <- prediction(pred,Validation$Type)
roc <- performance(lr_fitpred,"tpr","fpr")
plot(roc,col="blue",lwd=3,main="ROC Curve for Logistic Regression:BEST AIC")
abline(a=0,b=1,lwd=3,lty=2,col="red")


#create data frame of values 


perf <-as.data.frame(cbind(roc@alpha.values[[1]], 
                           roc@x.values[[1]], 
                           roc@y.values[[1]])) 
colnames(perf) <-c("Probability","FPR","TPR") 


#removing infinity value from data frame 

perf <-perf[-1,] 


#reshape the data frame 

install.packages("reshape")
library(reshape) 
perf2<- melt(perf, measure.vars = c("FPR", "TPR")) 


#Plotting FPR, TPR on y axis and cut-off probability on x axis 

library('ggplot2')

ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()



#model accuracy - Confusion Matrix

install.packages("SDMTools")
library(SDMTools)
confusion.matrix (Validation$Type, pred, threshold = 0.70)

source(choose.files())
Concordance(fit_test)


#Decile Scoring for Development dataset

train1<- cbind(Development, Prob=predict(fit_5, type="response")) 
View(train1)


#Creating Deciles

decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
train1$decile<-factor(train1$decile)


#Decile Analysis Reports

install.packages("sqldf")
library(sqldf)
fit_train_DA <- sqldf("select decile, min(Prob) as Min_prob
                      , max(Prob) as max_prob
                      , sum(Type) as Attrited_Count
                      , (count(decile)-sum(Type)) as Non_Attrited_Count 
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_5_Train_CE.csv",row.names = F)

#Decile Scoring for validation dataset

test1<- cbind(Validation, Prob=predict(fit_3,Validation, type="response")) 
View(test1)
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
test1$decile<-factor(test1$decile)
names(test1)

write.csv(test1,"Fit5_Test_CE.csv",row.names = F)

#Decile Analysis Reports


fit_test_DA <- sqldf("select decile, min(Prob) as Min_prob
                     , max(Prob) as max_prob
                     , sum(Type) as Attrited_Count
                     , (count(decile)-sum(Type)) as Non_Attrited_Count 
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit5_test_CE.csv",row.names = F)

#Prediction for Att for Validation data set

New_Employee<-cbind(Validation, Prob=predict(fit_5, newdata=Validation, type="response"))
View(New_Employee)
New_Employee$Type_New <- ifelse(New_Employee$Prob>0.70, 1,0)
sum(New_Employee$Type_New)


#Customer Profiling using Deciles

Employee_Profile<-data.frame(rbind(train1,test1))
write.csv(Employee_Profile,"Fit_5_CE_EMployee_profile.csv",row.names = F)                              


#You can also exponentiate the coefficients and interpret them as odds-ratios.

# odds ratios only

exp(coef(fit_4))


## odds ratios and 95% CI
odds <-exp(cbind(OR = coef(fit_5), confint(fit_5)))

write.csv(odds, "odds.csv")
          
          