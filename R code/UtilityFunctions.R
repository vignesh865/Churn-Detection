#Generates histogram and boxplot and saves in the given path 
generateOutlierImage = function(length, cnames, data, nameSuffix, imagePath, x_axis ){
  
  setwd(imagePath)
  
  for(index in 1:length){
    print(cnames[index])
    assign(paste0("plot",index), ggplot(aes_string(y=(cnames[index]), x=x_axis),data = data)
           +stat_boxplot(geom = "errorbar",width = 0.5)
           +geom_boxplot(outlier.color = "red", fill="grey", outlier.shape = 18, outlier.size = 1, notch = F)
           +theme(legend.position = "bottom")+labs(y=cnames[index], x=x_axis)
    );
    
    assign(paste0("histogram",index), 
           ggplot(data, aes_string(x=cnames[index]))+
             geom_histogram(fill="DarkSlateBlue",colour = "black") + geom_density() + theme_bw()+ 
             xlab(cnames[index])+ylab("Freq")+
             scale_x_continuous(breaks = pretty_breaks(n=6))+
             scale_y_continuous(breaks = pretty_breaks(n=10)) +
             theme(text = element_text(size = 15))
    );
    ggsave(paste0(cnames[index],nameSuffix),plot = gridExtra::grid.arrange(get(paste0("histogram",index)),get(paste0("plot",index)), ncol=2), device = NULL, height=9,width=12,dpi=72)  
  }
}

#This method assign levels to the factors
assignLevels = function(data){
  for(i in 1:ncol(data)){
    if(class(data[,i])=="factor"){
      data[,i]=factor(data[,i],labels = (1:length(levels(data[,i]))))
    }
  }
  
  return(data)
}

#Replace NA values with median value
medianImputation = function(data, cnames){
  for (nnames in cnames){
    data = internalImputation(data, nnames, median(data[, nnames]))
  }
  return(data)
}

#Replace NA values with mean value
meanImputation = function(data, cnames){
  for (nnames in cnames){
   data = internalImputation(data, nnames, mean(data[, nnames]))
  }
  return(data)
}

#Helper method for mean and median imputation
internalImputation = function(data,nnames, replacementValue){
    val = boxplot$stats(data[, nnames])$out
    data[data[,nnames] %in% val,nnames]= replacementValue
    return(data)
}

#Remove given features from data frame
removeFeatures = function(data, varaibleList){
  for (variable in varaibleList) {
    data[variable] = NULL
  }
  
  return(data)
}

#Feature scaling standardization
standardizeData = function(data){
  numericNames = sapply(data, is.numeric)
  numeric_data = data[, numericNames]
  for (colname in colnames(numeric_data)) {
    data[,colname] = (data[,colname] - min(data[,colname]))/(max(data[,colname])-min(data[,colname]))
  }
  return(data)
}

#Feature scaling normalization
normalizeData = function(data){
  numericNames = sapply(data, is.numeric)
  numeric_data = data[, numericNames]
  for (i in colnames(numeric_data)) {
    data[,i]=(data[,i]-mean(data[,i]))/(sd(data[,i]))
  }
  return(data)
}

imputeMissingValues = function(Data, strategy){
  numericNames = sapply(Data, is.numeric)
  numeric_data = Data[, numericNames]
  numericNames = colnames(numeric_data)
  
  print(numeric_data)
  for (num_col in numericNames){
    Data[num_col][is.na(Data[num_col])]=mean(Data[num_col],na.rm = T)
  }
  
  return(Data)
}
