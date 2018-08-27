

# Data Loading and Splitting
library(caret)

??createDataPartition

data("mtcars")

Split <- createDataPartition(mtcars$mpg, p=0.6, list = FALSE)

dev <- mtcars[Split,]
val <- mtcars[-Split,]


lmfit <- train(mpg ~., data = dev, method = "lm")

summary(lmfit)


??trainControl()



ctrl<-trainControl(method = "cv",number = 10)

lmCVFit<-train(mpg ~ ., data = mtcars, method = "lm", trControl = ctrl, metric="Rsquared")

summary(lmCVFit)


# Important function of Caret package

varImp(lmfit)
plot(varImp(lmfit))


predictedVal<-predict(lmfit,val)

modelvalues <- data.frame(obs = val$mpg, pred=predictedVal)

defaultSummary(modelvalues)


setwd("C:/Users/AH0667765/Desktop/Prac Loa Pred")
train<-read.csv("train_u6lujuX_CVtuZ9i.csv",stringsAsFactors = T)

str(train)

# missing values

sum(is.na(train)) # total missing values
colSums(is.na(train)) # Missing values in each column


# Next, let us use Caret to impute these missing values using KNN algorithm. 
# We will predict these missing values based on other attributes for that row. 
# Also, we'll scale and center the numerical data by using the convenient preprocess() in Caret.

#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(train, method = c("knnImpute","center","scale"))


install.packages("RANN")
library('RANN')

train_processed <- predict(preProcValues, train)

View(train)
View(train_processed)

sum(is.na(train_processed))
#[1] 0

# It is also very easy to use one hot encoding in Caret to create dummy variables for each level of a categorical 
# variable. But first, we'll convert the dependent variable to numerical.


#Converting outcome variable to numeric
train_processed$Loan_Status<-ifelse(train_processed$Loan_Status=='N',0,1)

id<-train_processed$Loan_ID
train_processed$Loan_ID<-NULL


str(train_processed)


# Now, creating dummy variables using one hot encoding:

#Converting every categorical variable to numerical using dummy variables

dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
View(train_transformed)
str(train_transformed)


#Converting the dependent variable back to categorical

train_transformed$Loan_Status<-as.factor(train_transformed$Loan_Status)


# Here, "fullrank=T" will create only (n-1) columns for a categorical column with n different levels. 
# This works well particularly for the representing categorical predictors like gender, married, etc. 
# where we only have two levels: Male/Female, Yes/No, etc. because 0 can be used to represent one class 
# while 1 represents the other class in same column.



# Splitting data using caret

#Spliting training set into two parts based on outcome: 75% and 25%

index <- createDataPartition(train_transformed$Loan_Status, p=0.75, list=FALSE)

trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]

#Checking the structure of trainSet
str(trainSet)


# Feature selection using Caret

#Feature selection using rfe in caret

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = TRUE)

outcomeName<-'Loan_Status'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)
Loan_Pred_Profile
#Recursive feature selection
#Outer resampling method: Cross-Validated (10 fold, repeated 3 times)
#Resampling performance over subset size:
#  Variables Accuracy  Kappa AccuracySD KappaSD Selected
#4   0.7737 0.4127    0.03707 0.09962        
#8   0.7874 0.4317    0.03833 0.11168        
#16   0.7903 0.4527    0.04159 0.11526        
#18   0.7882 0.4431    0.03615 0.10812        
#The top 5 variables (out of 16):
#  Credit_History, LoanAmount, Loan_Amount_Term, ApplicantIncome, CoapplicantIncome
#Taking only the top 5 predictors
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome", "CoapplicantIncome")



# Training models using Caret

names(getModelInfo())


# We can simply apply a large number of algorithms with similar syntax. 
# For example, to apply, GBM, Random forest, Neural net and Logistic regression :


model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
summary(model_rf)

model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
summary(model_rf)


model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
summary(model_glm)


fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)


# Using tuneGrid
modelLookup(model='gbm')


grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

# training the model
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)

# summarizing the model
print(model_gbm)
plot(model_gbm)


#using tune length
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
print(model_gbm)

#Stochastic Gradient Boosting
#461 samples
#5 predictor
#2 classes: '0', '1'

#No pre-processing
#Resampling: Cross-Validated (5 fold, repeated 5 times)
#Summary of sample sizes: 368, 369, 369, 370, 368, 369, ...
#Resampling results across tuning parameters:

#  interaction.depth  n.trees  Accuracy   Kappa   
#1                  50      0.7978084  0.4541008
#1                 100      0.7978177  0.4566764
#1                 150      0.7934792  0.4472347
#1                 200      0.7904310  0.4424091
#1                 250      0.7869714  0.4342797
#1                 300      0.7830488  0.4262414
...
#10                 100      0.7575230  0.3860319
#10                 150      0.7479757  0.3719707
#10                 200      0.7397290  0.3566972
#10                 250      0.7397285  0.3561990
#10                 300      0.7362552  0.3513413
#10                 350      0.7340812  0.3453415
#10                 400      0.7336416  0.3453117
#10                 450      0.7306027  0.3415153
#10                 500      0.7253854  0.3295929


plot(model_gbm)


#Checking variable importance for GBM

#Variable Importance
varImp(object=model_gbm)

#Plotting Varianle importance for GBM
plot(varImp(object=model_gbm),main="GBM - Variable Importance")




#Checking variable importance for RF
varImp(object=model_rf)


#Plotting Varianle importance for Random Forest
plot(varImp(object=model_rf),main="RF - Variable Importance")


#Checking variable importance for NNET
varImp(object=model_nnet)

#Checking variable importance for GLM
varImp(object=model_glm)

#Predictions
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)

confusionMatrix(predictions,testSet[,outcomeName])





