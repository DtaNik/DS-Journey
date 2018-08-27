# Data Importing
CE_Data <- read.csv("path")
View(CE_Data)

mydata <- CE_Data
# Data Summary
str(mydata, list.len  = 150)

# Column Names

colnames(mydata)
# 
# [1] "HC.ATT"                "Type"                  "Associate.ID"          "Business.Group..CL.1."
# [5] "Dlvry.Group..CL.3."    "Reporting.Mgr.ID"      "Job.Title"             "Aon.Pay.Level"        
# [9] "Location.Group"        "BU"                    "Tenure.In.Pay.Band"    "Tenure.In.Job.Code"   
# [13] "Voluntary"             "Gender"                "Resource.Type"         "Tenure.in.AON"        
# [17] "Age"                   "Engagement.Score"     

#Data Summary
str(mydata)

#Data Exploration and Data Understanding using Descriptive Analytics
library(psych)
library(ISLR) 
library(ggplot2)
library(caret)

describe(mydata$Age)

qplot(mydata$Engagement.Score, geom="histogram")


ggplot(data=mydata, aes(x = HC.ATT, y = Age)) + geom_boxplot() + 
  facet_wrap(~ Gender, ncol = 5)

ggplot(data=mydata, aes(x = HC.ATT, y = Engagement.Score)) + geom_boxplot() + 
  facet_wrap(~ Gender, ncol = 5)

qplot(Location.Group, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Business.Group..CL.1., data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Dlvry.Group..CL.3., data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Job.Title, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Aon.Pay.Level, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(BU, data=mydata, geom="bar",fill=factor(HC.ATT))  
qplot(Tenure.In.Pay.Band, data=mydata, geom="bar",fill=factor(HC.ATT))  
qplot(Tenure.In.Job.Code, data=mydata, geom="bar",fill=factor(HC.ATT))  
qplot(Gender, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Resource.Type, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Age, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Engagement.Score, data=mydata, geom="bar",fill=factor(HC.ATT))
qplot(Tenure.in.AON, data=mydata, geom="bar",fill=factor(HC.ATT))


# Randomly choosing development sample with Non-Attrited(Good - Type 0) and Attrited (Bad - Type 1)

set.seed(123)
library(caTools)
train_ind <- sample(1:nrow(mydata), size = floor(0.70 * nrow(mydata)))

Development<-mydata[train_ind,]
Validation <-mydata[-train_ind,]

table(Development$Type)
str(Development)
table(Validation$Type)
str(Validation)

# Different fits for the robust models

fit <- step(glm(formula = Type ~ Business.Group..CL.1. + Dlvry.Group..CL.3.+ Tenure.In.Job.Code
                + Tenure.in.AON + Age + Engagement.Score, family=binomial,data = mydata))
summary(fit)

#_________________________________________________________

# Picking fit_5 for the time being

summary(fit_5)
ls(fit_5)
model_fit<-fit_5$model
coeff<-fit_5$coef 
write.csv(coeff, "coeff.csv")

# Anova 

anova(fit, test = 'Chisq')

# Wald Test

install.packages("aod")
library(aod)

wald.test(b= coef(fit_5), Sigma = vcov(fit_5), Terms = 4)
#_________Output_________________________________________________________
# Wald test:
----------
  
  #  Chi-squared test:
  # X2 = 253.0, df = 3, P(> X2) = 0.0
  
  #The chi-squared test statistic of 253.0, with three degrees of freedom 
  #is associated with a p-value of 0.0 indicating that the overall effect 
  #  of compa is statistically significant.
  
  # similarly we test the same for other variables  
  
  #__________________________________________________________________________


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

ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()


#model accuracy - Confusion Matrix

install.packages("SDMTools")
library(SDMTools) 
confusion.matrix (Development$Type, pred, threshold = 0.50)

Concordance 

source(choose.files())
Concordance(fit_5)


#odds ratios and 95% CI 

exp(cbind(OR = coef(fit_5), confint(fit_5))) 

#Multicolleniarity Check

library(car)

exp(cbind(OR = coef(fit_5) ,confint(fit_5))) 
vif(fit_5)

#In real life, when we deal with high number of observations, often experts urge that it is not of a major issue if multicollinearity is not completelt removed. This is because as the number of observations go high, in lakh or million, impact of multicollinearity comes down.


#Autocorrelation Check

durbinWatsonTest(fit_5)


#Prediction on Validation 


fit_test <- glm(formula = Type ~Compa+ Years.In.Service +Ops.Tech, family=binomial,data = Validation)
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
confusion.matrix (Validation$Type, pred, threshold = 0.50)

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

write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)

#Decile Scoring for validation dataset

test1<- cbind(Validation, Prob=predict(fit_final,Validation, type="response")) 
View(test1)
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
test1$decile<-factor(test1$decile)
names(test1)

write.csv(test1,"test_1.csv",row.names = F)

#Decile Analysis Reports


fit_test_DA <- sqldf("select decile, min(Prob) as Min_prob
                     , max(Prob) as max_prob
                     , sum(Type) as Attrited_Count
                     , (count(decile)-sum(Type)) as Non_Attrited_Count 
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)

#Prediction for Att for Validation data set

New_Customer<-cbind(Validation, Prob=predict(fit_5, newdata=Validation, type="response"))
View(New_Customer)
New_Customer$Type_New <- ifelse(New_Customer$Prob>0.50, 1,0)
sum(New_Customer$Type_New)


#Customer Profiling using Deciles

Customer_Profile<-data.frame(rbind(train1,test1))
write.csv(Customer_Profile,"Customer_Profile.csv",row.names = F)                              


#You can also exponentiate the coefficients and interpret them as odds-ratios.

# odds ratios only

exp(coef(fit_5))


## odds ratios and 95% CI
odds <-exp(cbind(OR = coef(fit_5), confint(fit_5)))

write.csv(odds, "odds.csv")

#Now we can say that for a one unit increase in Compa1, the odds of being attrited (versus not being attrited) increase by a factor of 2.3. 



#the odds ratio for the intercept is not generally interpreted.





