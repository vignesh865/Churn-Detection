#Training data - Synthetic_data.csv
#Testing data - Test_data.csv
# Output - Test data prediction and metrics

rm(list=ls())

setwd("G://ml//edwiser//churn")

x=c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","C50","dummies","e1071","MASS","rpart","gbm","ROSE","scales")

lapply(x,require,character.only=TRUE)

rm(x)
source("UtilityFunctions.R")


train = read.csv("Synthetic_data.csv")

#Change to appropriate data type
train$international.plan = as.factor(train$international.plan)
train$voice.mail.plan = as.factor(train$voice.mail.plan)
train$Churn = as.factor(train$Churn)

test = read.csv("Test_data.csv",na.strings = c(""," ", "na"))

#removed at coorelation analysis
test = removeFeatures(test,c("total.day.minutes", "total.eve.minutes", "total.night.minutes","total.intl.minutes"))

#Removed using Chi-square
test=subset(test, select = -c(area.code, phone.number))

#Assign factor value to levels
test = assignLevels(test)


#Selected by var imp selection
train = removeFeatures(train, c('state','account.length'))
test = removeFeatures(test, c('state','account.length' ))

str(train)
str(test)

##Outlier analysis
cnames=colnames(train[, sapply(train, is.numeric)])
train = medianImputation(train, cnames)

#Feature Scaling
train = normalizeData(train)
test = normalizeData(test)

#Model
com = rbind(train, test)
RandomF_Model = randomForest(Churn ~., com[1:6666,], importance = TRUE,ntree = 500)
RF_Predictions = predict(RandomF_Model, com[6667:8333,c(-13)])
RF_table = table(com[6667:8333,13],RF_Predictions)

#Model output
confusionMatrix(RF_table)
