rm(list=ls())

setwd("G://ml//edwiser//churn")

x=c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","C50","dummies","e1071","MASS","rpart","gbm","ROSE","scales")

lapply(x,require,character.only=TRUE)

rm(x)
source("UtilityFunctions.R")

train = read.csv("Train_data.csv",na.strings = c(""," ", "na"))
test = read.csv("Test_data.csv",na.strings = c(""," ", "na"))

summary(train)
str(test)
str(train)

##Changing to appropriate type
train$area.code = as.factor(train$area.code)
test$area.code = as.factor(test$area.code)


#Assign numbers to factors
train = assignLevels(train)
test = assignLevels(test)



## Missing values - No missing values 
missingValues=data.frame(apply(train, 2,function(x){sum(is.na(x))}))
colnames(missingValues) = "Missing value"
missingValues=data.frame(apply(test, 2,function(x){sum(is.na(x))}))
sum(is.na(train))

#Feature Selection
#All the feature selection analysis has been done at Churn prod.R file itself.
#Hence directly selecting features because correlation will be same for synthetic data and training data

#Correlation analysis
train = removeFeatures(train, c("total.day.minutes", "total.eve.minutes", "total.night.minutes","total.intl.minutes"))
test = removeFeatures(test,c("total.day.minutes", "total.eve.minutes", "total.night.minutes","total.intl.minutes"))

#Removed using Chi-square
train=subset(train, select = -c(area.code, phone.number))
test=subset(test, select = -c(area.code, phone.number))


#Synthetic data generation
library(ROSE)
#generated data with Random over sampling method
generated = ROSE(Churn ~., data=train,seed = 3)$data

#binding generated data and training data
train = rbind(train, generated)

#writing this newly generated syntheic data to csv file
write.csv(train,"Synthetic_data.csv", row.names = FALSE)

table(train$Churn)


#Selected by var imp selection
train = removeFeatures(train, c('state','account.length'))
test = removeFeatures(test, c('state','account.length' ))

##Outlier analysis

cnames=colnames(train[, sapply(train, is.numeric)])

#Imputation methods - median is most suitable among all
train = medianImputation(train, cnames)

#Feature Scaling 0 normalization is suitable as it was an normaly distributed data
train = normalizeData(train)
test = normalizeData(test)


#Model
com = rbind(train, test)
RandomF_Model = randomForest(Churn ~., com[1:6666,], importance = TRUE,ntree = 500)
RF_Predictions = predict(RandomF_Model, com[6667:8333,c(-13)])
RF_table = table(com[6667:8333,13],RF_Predictions)

#Model output
confusionMatrix(RF_table)

