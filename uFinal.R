
# I will be using Random forest model for final submission.

# For variable selection and modelling work see file 'uModels.R'

# For EDA see file 'uEda.R'

# In this processed data has been used , Both train and test have been transfromed 
# following same logic, see file 'uProc.R'

# Some facts about orignal data

# Missing data was not random, mainly it was for children

# puma variable is similar to zip codes, So using information in it is very important

# Could not find relation of missing values in povpip, so imputed with trimmed mean

# I have use buckets for age, I have extensively used glmnet model for variable selection

# Since scoring is with F1 score so i have used a custom metric for it


# Due to time constraints I was not able to do more with modelling part

# I am relying on random forests because of its relaibility

# In future work will try Gradient boosting and Neural nets using python because I like 
#it more for modelling part...

# I try to create best data for linear models, cuz they take very less time to fit..

# Enjoyed a lot working on this dataset..I am hopeful that it will 
#keep me busy for the spring break.


#loading packages
library(jsonlite)
library(tidyverse)
#caret for modelling
library(caret)
library(caretEnsemble)
# F1 meric
library(MLmetrics)

#reading data

train <- read_csv('train_prep.csv')
#glimpse(train)
#summary(train)

#Converting target names to R reradable names for levels, i.e removing space
train$hicov=ifelse(train$hicov == "Without Healthcare","Without_H","With_H")

#converting to factor
train$hicov <- as.factor(train$hicov)

#Dropping id
train$id <- NULL

#Modelling
# F1 metric for caret 
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[2])
  c(F1 = f1_val)
}

#Control for model
myControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = f1,
  classProbs = TRUE,
  verboseIter = TRUE
  #sampling = 'smote'
)

# I have used class weights and smote sampling to handle imbalance
# smote worked little bit better but it is doesnt fit due to computational limits 
# So will use weights to handle imbalance
model_weights <- ifelse(train$hicov == "With_H" ,
                        (1/table(train$hicov)[1]) * 0.5,
                        (1/table(train$hicov)[2]) * 0.5)


#Grid for random forest, mtry = 10 worked best with 0.7 sample of data
# Have only worked on tuning mtry, 



myGrid = data.frame(mtry = c(10),splitrule = "gini",min.node.size=1)
set.seed(27)
modelrf2<-train(hicov~.,data=train,
                metric = "F1",
                method="ranger",
                weights = model_weights,
                tuneGrid=myGrid,
                trControl = myControl)

modelrf2
#F1 0.3140


#Importing test set


test <- read_csv('test_prep.csv')

submission <- data.frame(id=as.character(test$id))

#dropping id
test$id <- NULL 

#predicting probs
prob<-predict(modelrf2,test,type='prob')

head(prob)

# as per local cv F1 score on test was max if we take cutoff as 0.35
p_class<-ifelse(prob[1]>0.38,'With_H','Without_H')

# hopefully it fits well

submission$pred<-p_class

#changing names back to as they were orignally
submission$pred=ifelse(submission$pred == "Without_H","Without Healthcare","With Healthcare")

#saving as json

subJson<- toJSON(submission,pretty=TRUE)

#writing file
write(subJson, "submissionTaran.json")







