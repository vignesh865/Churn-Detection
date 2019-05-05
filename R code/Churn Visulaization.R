
#Outlier analysis
generateOutlierImage(length(numeric_data), cnames, train, "With outlier.jpg", "G://ml//edwiser//churn//with_outlier")
generateOutlierImage(length(numeric_data), cnames, train, "Without outlier.jpg", "G://ml//edwiser//churn//without_outlier")


# Understanding data - Deriving facts in feature selection #report
setwd("G://ml//edwiser//churn//facts")
ggsave(paste0("international plan"," Vs Churn.jpg"),plot = plot(Churn~international.plan,data = train, main = "Churn Vs International Plan", xlab = "International plan", col=colors()[125:25])
       , device = NULL)  
ggsave(paste0("voice mail plan"," Vs Churn.jpg"),plot = plot(Churn~voice.mail.plan,data = train, main = "Churn Vs Voice mail", xlab = "Voice mail plan", col=colors()[125:25])
       , device = NULL)  
numeric_index = sapply(train, is.numeric)
numeric_data = train[, numeric_index]
cnames=colnames(numeric_data)
for(index in 1:length(numeric_data)){
  ggsave(paste0(cnames[index]," Vs Churn.jpg"),plot = plot(train$Churn~cut(numeric_data[,index],10),data = numeric_data, main = paste("Churn vs ",cnames[index]), xlab = cnames[index], col=colors()[125:25]), device = NULL)  
}

#Roc curve for model output
roc.curve(logit_out,test[,10], main = "Logistic Regression ROC")
roc.curve(Dt_prediction , test[,10], main = "Decision Tree ROC")
roc.curve(RF_Predictions,test[,10], main = "Random forest ROC")


#Synthetic data ROC
roc.curve(RF_Predictions,test[,13], main = "Random forest ROC")
