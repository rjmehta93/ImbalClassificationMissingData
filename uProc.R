
#############Data Preparation############

library(jsonlite)
library(tidyverse)
library(caret)

train <- fromJSON("train.json")
test <- fromJSON("test.json")



# separating id and target variables
train_id <- data.frame(id=as.character(train$id))
train_labels <- data.frame(hicov=train$hicov)
test_id <- data.frame(id=as.character(test$id))
train$id <- NULL
train$hicov <- NULL
test$id <- NULL


df_all <- rbind(train, test)


fac <- lapply(df_all, class) == "character"
df_all[, fac] <- lapply(df_all[, fac], as.factor)

#Imputing missing values
# The logic for choosing these values is explained in EDA file

#new level for esr as kid
df_all$esr = factor(df_all$esr, levels=c(levels(df_all$esr), 'kid'))
df_all$esr[is.na(df_all$esr) & df_all$agep < 15 ] <- 'kid'

#for the remaining missing in esr using most common level
df_all$esr[is.na(df_all$esr) & df_all$agep == 15 ] <- 'Employed'

# new level in schl as mid schl
df_all$schl = factor(df_all$schl, levels=c(levels(df_all$schl), 'Mid School'))

df_all$schl[is.na(df_all$schl) & df_all$agep < 15 ] <- 'Mid School'

#pincp i.e personal income for children is dependent on parents 
#so taking replacing missing vals with  trimmed mean
df_all$pincp[is.na(df_all$pincp) & df_all$agep < 15 ] <- mean(df_all$pincp,na.rm=T,trim=0.2)

#Wkhp has 66k missing values for children imputing with 0

#wkhp missing are not working so value for  all should be zero
# Imputing low values like 0,5,10 can be done for different levels of esr

df_all$wkhp[is.na(df_all$wkhp) ] <-  0


#330 seems unbiased for povpip   # diff values for Povpip imputation can be checked
df_all$povpip[is.na(df_all$povpip)] <- 330

#retired income for children should be zero
df_all$retp[is.na(df_all$retp) ] <- 0

#public assistance for children as median which is 0 or mean can be used
df_all$pap[is.na(df_all$pap) ] <- 0

#similar for interest income, shpuld be zero for children
df_all$intp[is.na(df_all$intp) ] <- 0

# creating new vars, Have used a bucket for retp
df_all$myRetp<-ifelse((df_all$retp > 0.1 & df_all$retp <= 21000) ,0,1)

#Avg of incomes
df_all$incSums<- (df_all$retp + df_all$pap+df_all$intp)/3

# Age buckets we can see boundaries in the plots, 

df_all$age15<-ifelse(df_all$agep<=15,0,1)
df_all$age56<-ifelse(df_all$agep>=56,0,1)
df_all$age22<-ifelse((df_all$agep >=22 & df_all$agep<=42 ),0,1)



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
df_all$p109<- ifelse(df_all$puma == 109, 1,0 )
df_all$p8507<- ifelse(df_all$puma == 8507, 1,0 )
df_all$p1304<- ifelse(df_all$puma == 1304, 1,0 )
df_all$p3760<- ifelse(df_all$puma == 3760, 1,0 )
df_all$p11601<- ifelse(df_all$puma == 11601, 1,0 )
df_all$p6710<- ifelse(df_all$puma == 6710, 1,0 )
df_all$p5903<- ifelse(df_all$puma == 5903, 1,0 )

#Group for vraiable in top 20
groupP<-c(101,5902,7311,7505,8104,11609,7114,3733,7505,2901,8512,11607,11502,11608,8501)
#group 98 has high importance

#Creating a new coulmn for above group
df_all$grP<- ifelse(df_all$puma %in% groupP, 1,0 )



#varaibles to drop
#different combinations for drop have been tried
drop <- c('retp','agep','puma','intp','st')
df_all <- df_all %>% select(-one_of(drop))


# encoding and converting to numeric 

df_all$vet = ifelse(df_all$vet == "Not Veteran",0,1)
df_all$deye = ifelse(df_all$deye =="Yes",0,1)
df_all$dear = ifelse(df_all$dear == "Yes",0,1)
df_all$sex = ifelse(df_all$sex == "Female",0,1)
df_all$cit = ifelse(df_all$cit == "Not citizen",0,1)



# craeting levels-1 dummy vars for factors to avoid multicollinearity

dmy <- dummyVars(" ~ .", data = df_all, fullRank=T)
df_all <- data.frame(predict(dmy, newdata = df_all))

#new interaction variables 
df_all$comb <- df_all$deye * df_all$dear
df_all$race1 <- df_all$race.Amer..Indian.alone + df_all$esr.Unemployed
df_all$race2 <- df_all$race.Amer..Indian.alone + df_all$age56
df_all$race3 <- df_all$mar.Separated + df_all$esr.Unemployed+
  df_all$cit + df_all$race.Amer..Indian.alone


# range To convert continous into 0-1 scale....

dProc<-preProcess(df_all, method = c("range"))

df_all<-predict(dProc, df_all)

#Complete

#glimpse(df_all)

#summary(df_all)

#recreating train and test
ntrain <- nrow(train)
train <- df_all[1:ntrain, ]
test <- df_all[(ntrain+1):nrow(df_all), ]

train <- data.frame(id = train_id, train, hicov = train_labels)
test <- data.frame(id = test_id, test)


#Saving locally
write_csv(train, "train_prep.csv")
write_csv(test, "test_prep.csv")






