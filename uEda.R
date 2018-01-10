
#EDA on Urbint data

###########################Part 1###############################

getwd()
#Loading packages
#jsonlite for reading json 
library(jsonlite)
#tisyverse to load all wickham family packages
library(tidyverse)
#themes for plots
library(ggthemes)

#importing data

train <- fromJSON("train.json")

#391282 obs and 20 vars
#seems generated numbers in intp, pap, 
#Initial data exploration of train and test

#lets look at the data and datatypes

glimpse(train)
View(train)

#converting character vars to strings

fac <- lapply(train, class) == "character"
train[, fac] <- lapply(train[, fac], as.factor)

#summarising train 
summary(train)

#saving output for future record
capture.output(summary(train), file = "summTrain.txt")
#First observations

#imbalanced binary classification 
ggplot(train,aes(x=hicov))+geom_bar()+thm+ggtitle("Target variable")

#prop of target
round(prop.table(table(train$hicov))*100)

# id's not in order, 
# missing vals in esr,schl, pincp,intp, pap, retp, povip, wkhp(nearly 50%) 
#agep min is 0, should be 16 as per info
# -ve in pincp
#encoding   


#proportion of miss vals

round(colSums(is.na(train))/nrow(train)*100,2)

# exploring vars

# Initial exploration test

test <- fromJSON("test.json")
# 97405 obs

glimpse(test)

#converting char to factors

#Checking for duplicates 
#
sum(duplicated(train))
#0
sum(duplicated(test))
#0

test[, fac] <- lapply(test[, fac], as.factor)
summary(test)
capture.output(summary(test), file = "summTest.txt")


#proportion of miss vals

round(colSums(is.na(test))/nrow(test)*100,2)
#same proportion of missing vals in columns in both test and train


#EDA 
#setting base theme
thm=theme_tufte()


#comparison of relation with target and training variables

#function to calculate proportion of categorical vars wrt Targget i.e hicov 

#calculating proportions from table and runding to 2 digits


catProp <- function(var) {
  round(prop.table(table(train$hicov,var),2)*100,2)
}

# factor vars 

# variation of target with fac vars 
lapply(train[,names(Filter(is.factor, train))],catProp)

catProp(as.factor(train$st))
#saving output
#capture.output(lapply(train[,names(Filter(is.factor, train))],catProp),file="catVarsProp.txt")

#Observations

# variables having significant difference with target vars
#vet,dear,sex,race,mar,
#citizenship has most impact

######################## plotting

catPlot <- function(var) {
  ggplot(train,aes_string(x=deparse(substitute(var)),fill="hicov"))+geom_bar(position="dodge")+thm+
    theme(axis.text.x=element_text(angle=45,hjust=1))
}

# plot of target with factor variables
catPlot(cit)
catPlot(race)
#ggplot(train,aes(x=vet))+geom_bar()+facet_wrap(~hicov)+thm

#Clearing environment
dev.off(dev.list()["RStudioGD"])



#####################Numerical data

#state should salso be categorical feature
#starting with non numerical features
levels(as.factor(train$st))
#3 levels for the state
#propotion of states with target
catProp(as.factor(train$st))


#Puma should also be categorical feature

nlevels(as.factor(train$puma))
#338 levels
#puma is similar to zip code,encode it as factor using string or 
#make it a binarized var using clusters
# another option is use buckets

#distribution for puma
ggplot(train,aes(x=puma,fill=as.factor(hicov)))+geom_density(alpha=0.5)+thm+ggtitle("Puma & target")


#increasing number of ticks in x axis to see which values have diff in distribution
ggplot(train,aes(x=puma,fill=hicov))+geom_density(alpha=0.5)+
  theme(axis.text.x=element_text(angle=60))+
  scale_x_continuous(breaks=round(seq(min(train$puma), max(train$puma), by = 600),1))

train %>% filter(puma>3000 &puma<6000) %>% 
ggplot(aes(x=puma,fill=hicov))+geom_density(alpha=0.5)+
  theme(axis.text.x=element_text(angle=60))+
  scale_x_continuous(breaks=round(seq(min(train$puma), max(train$puma), by =200),1))

#buckets<200,200 to 590,800,1200,1320,1720,2000,2870,

train$pumaFac<-as.factor(train$puma)
pumaRatio<-catProp(train$pumaFac)

pumaRow<-train %>% group_by(pumaFac) %>%  summarise(n())


#encode using 1500 as interval, thn use that as factor var
#groupings to see which has max diff



#ggplot(train,aes(x=puma,fill=hicov))+geom_density(alpha=0.5)+thm
#distribution of puma for train and test 
ggplot()+geom_density(data=test,aes(x=puma))+
  geom_density(data=train,aes(x=puma),color='blue')+
  thm+ggtitle("Puma dist for train and test data")
#  



#Age variable

#ggplot(train,aes(x=agep))+geom_histogram()+thm
#20 to 50 important differentiator
ggplot(train,aes(x=agep,fill=hicov))+geom_density(alpha=0.5)+thm+ggtitle("Age & hicov")
# range for people without hicov is less
# further investigating

ggplot(train,aes(x=agep,fill=hicov))+geom_density(alpha=0.5)+ggtitle("Age & hicov")+
  scale_x_continuous(breaks=round(seq(min(train$agep), max(train$agep), by = 4),1))+
  theme_fivethirtyeight()

train %>% filter(agep > 15 & agep<50) %>% 
ggplot(aes(x=agep,fill=hicov))+geom_density(alpha=0.5)+ggtitle("Age & hicov")+
  scale_x_continuous(breaks=round(seq(min(train$agep), max(train$agep), by = 4),1))+
  theme_fivethirtyeight()




#encoding for age with groups
#14,18,56
#distribution of age for train and test 
ggplot()+geom_density(data=test,aes(x=agep))+
  geom_density(data=train,aes(x=agep),color='blue')+
  thm+ggtitle("Agep dist for train and test data")
#Expectation is train should have higher peaks because of size, 
                     
summary(test$agep)

summary(train$agep)


#povpip

ggplot(train,aes(x=povpip,fill=hicov))+geom_density(alpha=0.5)+thm
# important var

#Exoloration of povpip

ggplot(train,aes(x=povpip,fill=hicov))+geom_density(alpha=0.5)+
  scale_x_continuous(breaks=round(seq(0, 500, by = 25),1))+
  theme(axis.text.x=element_text(angle=60))+
  ggtitle("Pivpop and Hicov")

#this shows clear intervals for hicov are 330,460
# high povpip has very high chance of having health care

ggplot()+geom_density(data=test,aes(x=povpip))+
  geom_density(data=train,aes(x=povpip),color='blue')+
  thm+ggtitle("povpip dist for train and test data")


#I am proceeding with the assumption that povpip for particular area should be same 
#and imputation of its missing values is most important, checking the realtion with puma
train %>% select(id,puma,povpip,hicov) %>% arrange(desc(puma)) %>% head(25)

# diff valus of povpip for same puma
train %>% select(id,puma,povpip,hicov) %>% arrange(desc(puma)) %>% tail(25)

#



#plotting a density plot

#high values  are very imp
ggplot(train,aes(x=puma,y=povpip))+ geom_point(alpha=0.1,shape='.',color='orange')+
  geom_density2d(color='blue',alpha=0.3)+thm+facet_grid(~hicov)+
  ggtitle("povpip and puma 2d density plot")


# povpip values and no of rows
train %>% select(puma,povpip)  %>% 
  group_by(as.factor(povpip)) %>%
  summarise(no_rows = n()) %>% arrange(desc(no_rows)) %>%
  head(10)
#there are 126261 rows with povpip as 501

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#getting mode of povpip by grouping puma hicov values 
modes1<- train %>% select(hicov,puma,povpip) %>% 
  filter(hicov=="With Healthcare") %>%
   group_by(as.factor(puma))  %>%  mutate(mode=getmode(povpip)) %>% 
  select(puma,mode) %>% unique() %>% arrange(mode)

#mode for all hicov values is 501

#mode values wihout hicov
modes2<- train %>% select(hicov,puma,povpip) %>% 
  filter(hicov!="With Healthcare") %>%
  group_by(as.factor(puma))  %>%  mutate(mode=getmode(povpip)) %>% 
  select(puma,mode) %>% unique() %>% arrange(mode)

# For without hicov,
#there is different mode values for povpip, and different 
# for 109 unique puma values 
# we can impute povpip using this

# Now working on income variable
# there can be high correlation in that

train %>% select(intp,pap,retp,pincp,hicov) %>% 
  filter(!is.na(intp)) %>% 
  group_by(hicov) %>% 
  summarise(mean(intp),mean(pap),mean(retp),mean(pincp))


train %>% select(wkhp,hicov) %>% 
  filter(!is.na(wkhp)) %>% 
  group_by(hicov) %>% 
  summarise(mean(wkhp))



#

ggplot(train,aes(x=pincp,fill=hicov))+geom_density(alpha=0.5)+thm

#pincp for <20000
train %>% filter(pincp<=20000) %>% ggplot(aes(x=pincp,fill=hicov))+geom_density(alpha=0.5)+
  #scale_x_continuous(breaks=round(seq(-1,1, by =0.01),1))+
  theme_fivethirtyeight()

# for imputing 0 is better


train %>% filter(pincp>=-20 & pincp<=20000) %>% ggplot(aes(x=pincp,fill=hicov))+geom_density(alpha=0.5)+
  scale_x_continuous(breaks=round(seq(0,20000, by =1000),1))+
  theme_fivethirtyeight()+theme(axis.text.x=element_text(angle=60))

#zooming in this way we can find buckts if we want to divide this variable
# its almost same for both, 1801,17001
#For impuatation.... 1801, can be used for imputation  

#ggplot(train,aes(x=pap,fill=hicov))+geom_density(alpha=0.5)+thm

#ggplot(train,aes(x=wkhp,fill=hicov))+geom_density(alpha=0.5)+thm
# missing vals To be cosidered later on
#pap should also be realted with puma
#ggplot(train,aes(x=pap,fill=hicov))+geom_density(alpha=0.5)+thm

summary(train$pap)
#Exploring pap
train %>% filter(pap<=2) %>% ggplot(aes(x=pap,fill=hicov))+geom_density(alpha=0.5)+
  scale_x_continuous(breaks=round(seq(-1,1, by =0.01),1))+
  theme_fivethirtyeight()

# Number of obseravations in buckets
train %>% filter(pap<=2) %>% summarise(n())

train %>% filter(pap>=2) %>% ggplot(aes(x=pap,fill=hicov))+geom_density(alpha=0.5)+
  #scale_x_continuous(breaks=round(seq(-1,1, by =0.01),1))+
  theme_fivethirtyeight()

train %>% filter(pap>=2 & pap<=100) %>% ggplot(aes(x=pap,fill=hicov))+geom_density(alpha=0.5)+
  scale_x_continuous(breaks=round(seq(0,100, by =5),1))+
  theme_fivethirtyeight()
train %>% filter(pap>=2 & pap<=100) %>% summarise(n())

train %>% filter(pap>=10000 ) %>% ggplot(aes(x=pap,fill=hicov))+geom_density(alpha=0.5)+
  #scale_x_continuous(breaks=round(seq(0,100, by =5),1))+
  theme_fivethirtyeight()

#From these plots we can see that the pap can be important variable

#72,500,4600 are boundaries


#similarly for retp and intp
train %>% filter(intp>=-2 & intp<=0.5) %>% ggplot(aes(x=intp,fill=hicov))+geom_density(alpha=0.5)+
  scale_x_continuous(breaks=round(seq(-1,1, by =0.01),1))+
  theme_fivethirtyeight()
#similar as pap


train %>% filter(intp>=-2 & intp<=0.5) %>% summarise(n())
#278658 rows 
#
#Correlation
cor(train$intp,train$pap,use = "na.or.complete")
#no correlation so both can be used

train %>% filter( retp>=10) %>% ggplot(aes(x=retp,fill=hicov))+geom_density(alpha=0.5)+
  scale_x_continuous(breaks=round(seq(0,150000, by =10000),1))+
  theme_fivethirtyeight()+theme(axis.text.x=element_text(angle=60))
#2000 for retp
train %>% filter(retp>=10) %>% summarise(n())
#278658 rows 

train %>% filter(retp<=-0.08 & retp>=0.08) %>% summarise(n())

cor(train[,c("intp","pap","retp",'pincp')],use = "na.or.complete")




#Lets see how the sum of these varies for the target

# The difference in mean show that this new variable can be used
train %>% select(intp,pap,retp,pincp,hicov) %>% 
  mutate(sumIncs=(intp+pap+retp+intp)/4) %>%  
  filter(!is.na(sumIncs)) %>% 
  group_by(hicov) %>% summarise(mean(sumIncs))





train %>% select(intp,pap,retp,pincp,hicov) %>% 
  mutate(sumIncs=(intp+pap+retp)/3) %>% filter(!is.na(sumIncs)) %>%  
  summarise(mean(sumIncs),max(sumIncs), min(sumIncs),median(sumIncs)) 

train$sumIncs<-(train$intp+train$pap+train$retp)/3

cor(train[,c("pincp","intp","pap","retp",'sumIncs')],use = "na.or.complete")


#sumInc has high corr with intp 

  
train %>% select(intp,pap,retp,pincp,hicov) %>% 
  mutate(sumIncs=(intp+pap+retp)/3) %>%  
  filter(sumIncs < 0.1 & sumIncs > -0.1) %>% 
  ggplot(aes(x=sumIncs,fill=hicov))+geom_density(alpha=0.5)
  
#avg of 3 is centered around 0 

train %>% select(intp,pap,retp,pincp,hicov) %>% 
  mutate(sumIncs=(intp+pap+retp)) %>%  
  filter(sumIncs < 10 & sumIncs > -10) %>% 
  ggplot(aes(x=sumIncs,fill=hicov))+geom_density(alpha=0.5)

train %>% select(intp,pap,retp,pincp,hicov) %>% 
  mutate(sumIncs=(intp+pap+retp)) %>%  
  filter(!is.na(sumIncs)) %>% 
  group_by(hicov) %>% summarise(mean(sumIncs))



ggplot()+geom_density(data=test,aes(x=pincp))+
  geom_density(data=train,aes(x=pincp),color='blue')+
  thm+ggtitle("pincp dist for train and test data")

#checking cor 
cor(train[,c("pincp","intp","pap","retp")],use = "na.or.complete")


################################Part 2

# Exploring relation between different variables
# For imputation of missing values
# Will try to see relationship between different variables 

 train %>%  select(hicov,esr,pincp) %>% 
   group_by(esr,hicov) %>% 
  summarise(avg=mean(pincp,trim=0.15,na.rm = FALSE),
            med=median(pincp),min_p=min(pincp),max_p=max(pincp),no=n())


train %>% filter(is.na(pincp)) %>%  select(esr,pincp) %>% 
   filter(!is.na(esr)) %>% summarise(n())

#NA in pincp only if NA in esr, so imputation as per levels of esr will not work

#so all esr na have pincp as na
#similar for school

train %>% filter(is.na(intp)) %>%  select(esr,pincp) %>% 
  filter(!is.na(esr)) %>% summarise(n())
#pap also na, similar for all income

#
pin_no <- train %>%  select(hicov,esr,pincp) %>% 
  filter(hicov!="With Healthcare") %>% group_by(esr) %>% 
  summarise(avg=mean(pincp,trim=0.15),
            med=median(pincp),min_p=min(pincp),max_p=max(pincp),no=n())


pinS <- train %>%  select(hicov,schl,pincp) %>% 
  filter(hicov=="With Healthcare") %>% group_by(schl) %>% 
  summarise(avg=mean(pincp,trim=0.15),
            med=median(pincp),min_p=min(pincp),max_p=max(pincp),no=n())

pinS_no <- train %>%  select(hicov,schl,pincp) %>% 
  filter(hicov!="With Healthcare") %>% group_by(schl) %>% 
  summarise(avg=mean(pincp,trim=0.15),
            med=median(pincp),min_p=min(pincp),max_p=max(pincp),no=n())

#Important observation for schl=="Less than HS", has missing values for pincp


#to confirm we can choose 
train %>% filter(schl=="Less than HS" & hicov =="With Healthcare" ) %>%  
  select(schl,pincp) %>% filter(!is.na(pincp)) %>% summarise(n(),mean(pincp))

train %>% filter(schl=="Less than HS" ) %>%  
  select(schl,pincp) %>% filter(!is.na(pincp)) %>% summarise(n(),mean(pincp))

train %>% filter(schl=="Less than HS" & hicov =="With Healthcare" ) %>%  
  select(schl,pincp) %>% summarise(n(),mean(pincp))


#Another finding for all schl == NA pincp is also NA
train %>% filter(is.na(schl)) %>%  select(schl,pincp) %>% 
  filter(!is.na(pincp)) %>% summarise(n())

# adding a column with number of na's in each row

train$numNa <- apply(train, 1, function(x) sum(is.na(x)))
summary(train$numNa)

nrow(train[train$numNa ==7, ])
#53888 with 6 na,12830 with 7
#13055 rows with more than 6 na's
#225 with  8 na

#For test
test$numNa <- apply(test, 1, function(x) sum(is.na(x)))
summary(test$numNa)

#pattern of missing values

train %>% filter(is.na(esr),is.na(schl),is.na(pincp)) %>% summarise(n())

#schl has 11970 missing values for all of these esr,wkhp, and pincp is also missing, 

train %>% filter(is.na(esr),is.na(schl),is.na(pincp),is.na(povpip)) %>% summarise(n())
#out of these povpip is missing for 225 that are rows with 8 missing values

#lets see if we can establish some relation with povpip
train %>% filter(is.na(esr),is.na(schl),is.na(pincp),!is.na(povpip)) %>% summarise(n())
#From these 11745 rows we need to see if they have some particular povpip values 
train %>% filter(is.na(esr),is.na(schl),is.na(pincp),!is.na(povpip)) %>%
  summarise(n_distinct(povpip),mean(povpip),median(povpip),min(povpip),max(povpip))
#we have 502 different values that means these missing have all the values of povpip,
#so we should not impute using povpip  


test %>% filter(is.na(esr),is.na(schl),is.na(pincp)) %>% summarise(n())
# its same for test


train %>% filter(is.na(esr),is.na(schl),is.na(pincp),is.na(wkhp)) %>% summarise(n())
#summary of rows with 8 miss vals, exploring to see some commons
train %>% filter(numNa==8) %>% summary()
# all non veterans,never married, all have agep less than 2, 
# children have missing vals

train %>% filter(numNa >= 6) %>% summary()
# saving this in a otuput file 
train %>% filter(numNa >= 6) %>% summary() %>% capture.output(file = "summChild.txt")

# so the source of all missing values is school children less than 14 years of age
test %>% filter(numNa >= 6) %>% summary()
#same for test

#1785/(65158+1785)  , 2.6% do not have coverage
# look at the other dataset
train %>% filter(numNa < 6) %>% summary()
# 22813/(301526+22813) 7% do not have coverage

#24598/(366684+24598)
#whole dataset has 6.2%
#train %>% filter(numNa < 6) %>% summary() %>% capture.output(file = "summAdult.txt")

# This is a kind of different dataset in a dataset, 
#One option can be to build models for these 2 kind of different datasets, 
# Model can identify this from agep variable
# I plan to have a new flag column for these and imputing intution based numbers
# 

# choosing values for imputing missing values

#esr
# creating a new level in esr for child
levels(train$esr)
catProp(train$esr)

# adding new level as kid

train$esr = factor(train$esr, levels=c(levels(train$esr), 'kid'))

train$esr[is.na(train$esr) & train$agep < 15 ] = 'kid'
summary(train$esr)

# for imputation of remainig NA's in esr

train %>% filter(is.na(train$esr)) %>% summary()
#all remaining NA's have age 15 and it seems they have outliers in pincp 

#-5400 for 15 year old is not viable
train %>% filter(is.na(train$esr)) %>% summarise(n_distinct(pincp))
#165 different values
train %>% filter(is.na(train$esr)) %>% select(pincp) %>% unique()

#Putting these 15 years into employed as it is the most common level

train %>% select(esr,agep) %>% group_by(esr) %>% summarise(mean(agep),n())


train$esr[is.na(train$esr) & train$agep == 15 ] = 'Employed'
#Train esr is imputed, now imputing school

train$schl = factor(train$schl, levels=c(levels(train$schl), 'Mid School'))

train$schl[is.na(train$schl) & train$agep < 15 ] = 'Mid School'


# Imputing pincp we will impute it with a value 
#which is not already present and or least common
# we have 48742 rows with zeros for pincp
train %>% select(pincp) %>% filter(pincp<=10) %>% 
  summarise(n_distinct(pincp),mean(pincp),median(pincp),n())
#only 54 values 
train %>% select(pincp) %>% filter(pincp<=10) %>% unique() %>% arrange(pincp)


train %>% select(pincp) %>% filter(pincp==16999) %>% 
  summarise(n()) 

#imputing it with 99

train$pincp[is.na(train$pincp) & train$agep < 15 ] = 99



#imputing wkhp for kids
train$wkhp[is.na(train$wkhp) & train$agep < 15 ] = 0


#imputing povpip as per mode of puma values
# povpip is differentiator
# mode for povpip is 501, 
#but 501 relates to yes hicovs
# if I use 501 for imputing it will push NA rows to yes hicov

# we can use 499 or will use 501 
train %>% select(puma) %>% filter(puma==499) %>% 
  summarise(n()) 
#puma doesnt have 499 as value so will use it


ggplot(data=train,aes(x=agep)) + 
  geom_bar(alpha=0.8,fill="orange") +
  ggtitle("Agep Distribution") + thm



summary(train$wkhp)

train %>% select(agep,wkhp) %>%
  filter(agep<15 & is.na(wkhp)) %>% summarise(n())


train %>% select(agep,wkhp) %>%
  filter(agep>15 & is.na(wkhp)) %>% summarise(n())

#imputing these 119219 is important, for kids it should be zero

train %>% select(agep,wkhp) %>%
  filter(agep>15 & is.na(wkhp)) %>% 
  summarise(mean(agep),median(agep),min(agep),max(agep))

#Max is 94 min is 16  median is 62
#so 
train %>% select(agep,wkhp) %>%
  filter(agep>15 & is.na(wkhp) & agep<55) %>% 
  summarise(mean(agep),median(agep),min(agep),max(agep),n())
#so 47000 are less than age 55 out of 120000 or so

#Different imptation for these
train %>% select(agep,wkhp,pincp) %>%
  filter(agep>15 & is.na(wkhp) & agep<55) %>% 
  summarise(mean(pincp),median(pincp),min(pincp),max(pincp),n())

#so mean pincp is very low
# so wkhp should also be low
summary(train$pincp)

train %>% select(agep,wkhp,pincp) %>%
  filter(is.na(wkhp) & agep>55) %>% 
  summarise(mean(pincp),median(pincp),min(pincp),max(pincp),n())

# For retired mean pincp is high but both these numbers should be low

#So 0 is valid value for wkhp 
summary(train$wkhp)


train %>% select(agep,wkhp,esr) %>%
  filter(agep>15 & is.na(wkhp)) %>% group_by(esr) %>% 
  summarise(n())


#so all are unemployed or not in labour force
  
