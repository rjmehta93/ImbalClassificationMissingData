# Modelling 

# Approach

# I plan to use Random forest and GLMnet for modelling, As they pair very well for ensembling 

# After trying this, will move to use xgboost and neural nets 

# For variable selection Boruta could have been used,

# For feature engineering, trained glmnet with different combinations of variables and differetnt tweaks on varaiables model 

# Due to large data set computation time is high To tackle this I have worked with 50 50 split for train and test, to decrease the training size

# For variable importance I have used caret's varimp. It returns imporatnce as measure of max absolute value of t statistic

# for dim reduction tried pca but acc decreased, generally it doesnt work well for classification


#Since the scoring metric is F1(harmonic mean of precicion recall) I have used custom metric in caret to score models

##################### Missing Values #######################

# Missing values are not random, Most of the misssing values are for age <15 
# and not employed people
# The values have been imputed accordingly using findings of EDA

# There are No Duplicate rows in dataset


##################### Imbalanced classification #####################

#To handle imbalance Smote sampling and class weights are used..

# Smote sampling gave better results, but it takes more time to fit

#Not tried up sampling

# I have created interaction variables

############CODE#################

getwd()
setwd( "/Users/bhumikasingh/Documents/Ds/Data Science Challenge")


#Loading packages
#jsonlite for reading json 
library(jsonlite)
#tisyverse to load all wickham family packages
library(tidyverse)
#caret for modelling
library(caret)
library(caretEnsemble)
# F1 meric
library(MLmetrics)

#Importing json file
train <- fromJSON("train.json")

#There are no duplicate values in train and test

######DATA PROCESSING

#converting chars to factors
fac <- lapply(train, class) == "character"
train[, fac] <- lapply(train[, fac], as.factor)

#Converting target names to R reradable names for levels, i.e removing space
train$hicov=ifelse(train$hicov == "Without Healthcare","Without_H","With_H")

#Imputing missing values
# The logic for choosing these values is explained in EDA file

#new level for esr as kid
train$esr = factor(train$esr, levels=c(levels(train$esr), 'kid'))
train$esr[is.na(train$esr) & train$agep < 15 ] <- 'kid'

#for the remaining missing in esr using most common level
train$esr[is.na(train$esr) & train$agep == 15 ] <- 'Employed'

# new level in schl as mid schl
train$schl = factor(train$schl, levels=c(levels(train$schl), 'Mid School'))

train$schl[is.na(train$schl) & train$agep < 15 ] <- 'Mid School'

#pincp i.e personal income for children is dependent on parents 
#so taking replacing missing vals with  trimmed mean
train$pincp[is.na(train$pincp) & train$agep < 15 ] <- mean(train$pincp,na.rm=T,trim=0.2)

#Wkhp has 66k missing values for children imputing with 0

#wkhp missing are not working so value for  all should be zero
# Imputing low values like 0,5,10 can be done for different levels of esr

train$wkhp[is.na(train$wkhp) ] <-  0

 
#330 seems unbiased for povpip   # diff values for Povpip imputation can be checked
train$povpip[is.na(train$povpip)] <- 330

#retired income for children should be zero
train$retp[is.na(train$retp) ] <- 0

#public assistance for children as median which is 0 or mean can be used
train$pap[is.na(train$pap) ] <- 0

#similar for interest income, shpuld be zero for children
train$intp[is.na(train$intp) ] <- 0

# creating new vars, Have used a bucket for retp
train$myRetp<-ifelse((train$retp > 0.1 & train$retp <= 21000) ,0,1)

#Avg of incomes
train$incSums<- (train$retp+train$pap+train$intp)/3

# Age buckets we can see boundaries in the plots, 

train$age15<-ifelse(train$agep<=15,0,1)
train$age56<-ifelse(train$agep>=56,0,1)
train$age22<-ifelse((train$agep >=22 & train$agep<=42 ),0,1)
# creating buckets for povpip
# accuarcy decreased so not using  it
#train$pov1<-ifelse(train$povpip<330,0,1)
#train$pov2<-ifelse(train$povpip>460,0,1)


###########Managing puma#############
#Approach 1
#Puma is a zip code like variable, so it should be treated as factor, 
#due to high number of levels, on encoding it increases model complexity 
# and training time, to handle this
#I have created new variables for the set of puma's which are indentified more with with_H
#Using catprop function in Eda File
# 

#In this I have used top 5 variables from log reg model if we run with only puma values as independependent variables
# for rest I have grouped
train$p109<- ifelse(train$puma == 109, 1,0 )
train$p8507<- ifelse(train$puma == 8507, 1,0 )
train$p1304<- ifelse(train$puma == 1304, 1,0 )
train$p3760<- ifelse(train$puma == 3760, 1,0 )
train$p11601<- ifelse(train$puma == 11601, 1,0 )
train$p6710<- ifelse(train$puma == 6710, 1,0 )
train$p5903<- ifelse(train$puma == 5903, 1,0 )

#Group for vraiable in top 20
groupP<-c(101,5902,7311,7505,8104,11609,7114,3733,7505,2901,8512,11607,11502,11608,8501)
#group 98 has high importance

#Creating a new coulmn for above group
train$grP<- ifelse(train$puma %in% groupP, 1,0 )



#target
target<-data.frame(train$hicov)

#varaibles to drop
#different combinations for drop have been tried
drop <- c('id','retp','hicov','agep','puma','intp','st')
train <- train %>% select(-one_of(drop))

# state and puma should be factor variables

# First modelling without puma as a factor because high number of levels
#train$st <- as.factor(train$st)


# encoding binary factors

# encoding and converting to numeric 

train$vet=ifelse(train$vet=="Not Veteran",0,1)
train$deye=ifelse(train$deye=="Yes",0,1)
train$dear=ifelse(train$dear=="Yes",0,1)
train$sex=ifelse(train$sex=="Female",0,1)
train$cit=ifelse(train$cit=="Not citizen",0,1)



# have not used dummy vars on the binary factors, cuz it changes names of the variable

#creating dummy variables 

# craeting levels-1 dummy vars for factors to avoid multicollinearity

dmy <- dummyVars(" ~ .", data = train, fullRank=T)
train <- data.frame(predict(dmy, newdata = train))

#new interaction variables 
train$comb <- train$deye*train$dear
train$race1 <- train$race.Amer..Indian.alone + train$esr.Unemployed
train$race2 <- train$race.Amer..Indian.alone + train$age56
train$race3 <- train$mar.Separated+train$esr.Unemployed+
  train$cit+train$race.Amer..Indian.alone


# by taking puma as factor and creating dummies 
#the number od vars has increased to 368 


# range To convert continous into 0-1 scale....

dProc<-preProcess(train, method = c("range"))

train<-predict(dProc,train)
#glimpse(train)

#Creating train test split
trainIndex <- createDataPartition(target$train.hicov, p = .7, 
                                  list = FALSE, 
                                  times = 1)

xTrain <- train[ trainIndex,]
xTest  <- train[-trainIndex,]

yTrain <-target[ trainIndex,"train.hicov"]
yTest <- target[ -trainIndex,"train.hicov"]

#removing train and target
remove(train,target)

# this function has been taken from stack overflow
#Function for F1 metric
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[2])
  c(F1 = f1_val)
}

#Control for model
myControl <- trainControl(
  method = "cv",
  number = 2,
  summaryFunction = f1,
  classProbs = T,
  verboseIter = T
)


#grid for parameter tuning for ridge and lasso params
#glmGrid = expand.grid(alpha = seq( 0.5, 1,length=10), lambda = seq(0.0001, 0.1, length = 1000))

# Weights for classes
model_weights <- ifelse(yTrain == "With_H" ,
                        (1/table(yTrain)[1]) * 0.5,
                        (1/table(yTrain)[2]) * 0.5)
set.seed(27)
myControl$sampling<-'smote'
myGrid = data.frame(mtry = c(8:10,12),splitrule = "gini",min.node.size=1)
modelrf<-train(x=xTrain ,y=yTrain,
               metric = "F1",
               method="ranger",
               #weights = model_weights,
               tuneGrid=myGrid,
               trControl = myControl)

pRng <- predict(modelrf,xTest )
(cRf<-confusionMatrix(pRng,yTest,mode= "everything",positive = "Without_H"))
#0.31218 with mtry 8
prob<-predict(modelrf,xTest,type='prob')
head(prob)

p_class<-ifelse(prob[1]>0.35,'With_H','Without_H')
(cRf<-confusionMatrix(p_class,yTest,mode= "everything",positive = "Without_H"))
#0.38 F1 0.33721,     0.34233 with 0.35   0.34352 0.33

#0.34 has 0.34386

p_class<-ifelse(prob[1]>0.34,'With_H','Without_H')
(cRf<-confusionMatrix(p_class,yTest,mode= "everything",positive = "Without_H"))



#Fixing seed
set.seed(27)
#Smote sampling
myControl$sampling<-'smote'
modelGlmnet<-train(x=xTrain ,y=yTrain,
                   metric = "F1",
                   method="glmnet",
                   #weights = model_weights,
                   #tuneGrid = glmGrid,
                   trControl = myControl)

#Variable impotance

#For linear models caret gives importance as per absolute value of T statistic

col_index <- varImp(modelGlmnet)$importance %>% 
  mutate(names=row.names(.)) %>%
  arrange(-Overall)

#saving
#write.csv(col_index,"impVarAgeB.csv",row.names=F)   



#ensembling
library(caretEnsemble)
model_list<-list(modelrf,modelGlmnet)
resamples<-resamples(model_list)


myControl <- trainControl(
  method = "cv",
  number = 2,
  summaryFunction = twoClassSummary,
  classProbs = T,
  verboseIter = T
)

model_weights <- ifelse(yTrain == "With_H" ,
                                       (1/table(yTrain)[1]) * 0.5,
                                       (1/table(yTrain)[2]) * 0.5)

modelList<-caretList(x=xTrain,y=yTrain,trControl=myControl,metric="ROC",weights = model_weights,
                     tuneList=list(
                       rf=caretModelSpec(method="ranger",tuneGrid=data.frame(mtry = c(2,3,4,6,8,12),splitrule = "gini",min.node.size=1)),  
                       glmnet=caretModelSpec(method="glmnet")
                     ))

modelList
#Random forest preictions are biased towards majority class, as mtry increaes spec decreses
#RF at mtry 2 is better option, Can try single tree
# variable selection is important
xyplot(resamples(modelList))

greedy_ensemble <- caretEnsemble(
  modelList, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)


glm_ensemble <- caretStack(
  modelList,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
summary(glm_ensemble)
#rf has p val significant

model_preds <- lapply(modelList, predict, newdata=xTest, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"With_H"])

model_preds$ensemble <- predict(glm_ensemble, newdata=xTest, type="prob")


#Ensemble is weighted average, that can be hand coded given sufficient time
#stacking is predicting target from predictions..
# Will work on addditional  modelling part with python for neural nets and xgboost 

# Faced technical issue with the predict function for ensemble objects so submitting single best model
# Need to see if this works on linux system, or will reinstall erlier version of R
# Due to my technical incompetence and shortage of time, I am not able to resolve this 
# 

#remove.packages("caretEnsemble")
#install.packages('caretEnsemble'(), dependencies = TRUE)
#devtools::install_github('zachmayer/caretEnsemble')
#library(caretEnsemble)

#Not working






