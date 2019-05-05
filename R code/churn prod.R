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
View(missingValues)
missingValues=data.frame(apply(test, 2,function(x){sum(is.na(x))}))
colnames(missingValues) = "Missing value"
View(missingValues)


##Outlier analysis
cnames=colnames(train[, sapply(train, is.numeric)])

#Imputation methods
train = medianImputation(train, cnames)
train = meanImputation(train, cnames)
train = knnImputation(train, k=7)


#Feature Selection
#Correlation analysis
corrgram(train[,cnames],order = F,upper.panel = panel.pie, text.panel = panel.txt,  main = "Coorelation plot")

train = removeFeatures(train, c("total.day.minutes", "total.eve.minutes", "total.night.minutes","total.intl.minutes"))
test = removeFeatures(test,c("total.day.minutes", "total.eve.minutes", "total.night.minutes","total.intl.minutes"))

#Chi-square test
factor_data = train[, sapply(train, is.factor)]

for (i in 1:5) {
  print(names(factor_data[i]))
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
}


train=subset(train, select = -c(area.code, phone.number))
test=subset(test, select = -c(area.code, phone.number))

#Variable importance by random forest
com = rbind(train, test)
RandomF_Model = randomForest(Churn ~., com[1:3333,], importance = TRUE,ntree = 500)

varImp(RandomF_Model)

train = removeFeatures(train, c('state','account.length','total.day.calls', 'total.eve.calls', 'total.night.calls' ))
test = removeFeatures(test, c('state','account.length','total.day.calls', 'total.eve.calls', 'total.night.calls' ))


#Feature Scaling

#Standardization
train = standardizeData(train)
test = standardizeData(test)

#Normalization
train = normalizeData(train)
test = normalizeData(test)


#Model selection
#Descision tree
c50_Model = C5.0(Churn ~., train, trails = 100, rules = TRUE)
summary(c50_Model)
Dt_prediction = predict(c50_Model, test[,-10],type = "class")
Dt_table = table(test[,10],Dt_prediction)
confusionMatrix(Dt_table)

#Random forest
com = rbind(train, test)
RandomF_Model = randomForest(Churn ~., com[1:3333,], importance = TRUE,ntree = 500)
RF_Predictions = predict(RandomF_Model, com[3334:5000,c(-10)])
RF_table = table(test[,10],RF_Predictions)
confusionMatrix(RF_table)


#Logistic Regression
logit_model = glm(Churn ~., data = train, family = "binomial")
summary(logit_model)
logit_prediction = predict(logit_model, newdata = test[, c(-10)], type = "response")
logit_out = ifelse(logit_prediction > 0.5, 2, 1)
logit_matrix = table(test[,10],logit_out)
confusionMatrix(logit_matrix)


#Final Judgment
Dt_table
confusionMatrix(Dt_table)
RF_table
confusionMatrix(RF_table)
logit_matrix
confusionMatrix(logit_matrix)
