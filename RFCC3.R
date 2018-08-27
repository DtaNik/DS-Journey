
#Random Forest CE

mydata

# Load the party package. It will automatically load other required packages.

install.packages('randomForest')
install.packages('party')

library(party)
library(randomForest)

# Randomly choosing development sample with Non-Attrited(Good - Type 0) and Attrited (Bad - Type 1)

set.seed(123)
library(caTools)
train_ind <- sample(1:nrow(mydata), size = floor(0.70 * nrow(mydata)))

Development<-mydata[train_ind,]
Validation <-mydata[-train_ind,]

table(Development$HC.ATT)
str(Development)
table(Validation$HC.ATT)
str(Validation)


# Create the forest.
output.forest <- randomForest(HC.ATT ~ Business.Group..CL.1. + Dlvry.Group..CL.3. + Job.Title + Aon.Pay.Level 
                              + Location.Group + BU + Tenure.In.Pay.Band + Tenure.In.Job.Code
                              + Gender + Resource.Type + Tenure.in.AON + Age + Engagement.Score,data = Development)
#View the forest results.
print(output.forest) 

#Importance of each predictor.
print(importance(output.forest,type = 2)) 

plot(output.forest)


# Variable Importance Plot
varImpPlot(output.forest,
           sort = T,
           main="Variable Importance",
           n.var=5)



Accuracy <- (99+443)/(99+27+2+443)
Accuracy

# Variable Importance Table
var.imp <- data.frame(importance(output.forest,type=2))

# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

Development$predicted.response <- predict(output.forest,Development)

# Confusion Matrix

# Load Library or packages

library(e1071)
library(caret)
confusionMatrix(data=Development$predicted.response,
                reference=Development$HC.ATT,
                positive='HC')

#It has accuracy of 97%, which is fantastic. Now we can predict response for the validation sample and calculate model accuracy for the sample.

# Predicting response variable
Validation$predicted.response <- predict(output.forest,Validation)

# Create Confusion Matrix
confusionMatrix(data=Validation$predicted.response,
                reference= Validation$HC.ATT,
                positive='HC')

#Accuracy level has dropped to 88% but still significantly higher.






MDSplot(output.forest,devlopment$HC.ATT)



