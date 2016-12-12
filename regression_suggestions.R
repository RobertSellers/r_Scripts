suggest<-function (v,target){
  suppressWarnings(require(moments))
  par(mfrow=c(2,3))
   sink("suggest.txt")
  i<-0
  for(j in names(v)){
    i<-i+1
    cat('----------------------------')
    cat("\n")
    cat(colnames(v)[i] )
    cat("\n")
    if(sapply(v[colnames(v)[i] ], class)=='integer'){
      remove_outliers(v[,i])
      cat(paste('Numeric', sapply(v[colnames(v)[i] ], class)))
      cat("\n")
      cat(paste("NA values: ", percent(sum(is.na(v[colnames(v)[i] ]))/nrow(v))))
      cat("\n")
      skew<-skewness(na.omit(v[colnames(v)[i] ]))
      cat(paste('Skewness Ratio: ',signif(skew,3)))
      cat("\n")
      if(skew>0){
        cat('Right-tailed skew')
        cat("\n")
        if(sum(is.na(v[colnames(v)[i] ]))/nrow(v)>0){
          cat('Consider converting NA to 0')
          cat("\n")
        }
        if(sum(is.na(v[colnames(v)[i] ]))/nrow(v)>.1){
          cat('Consider deleting variable')
          cat("\n")
        }
      }else{
        cat('Left-tailed skew')
        cat("\n")
        if(sum(is.na(v[colnames(v)[i] ]))/nrow(v)>0){
          cat('Consider converting NA to 0')
          colnames(v)[i] = 0
          cat("\n")
        }
        if(sum(is.na(v[colnames(v)[i] ]))/nrow(v)>.1){
          cat('Consider deleting variable')
          cat("\n")
        }
      }
      
      intPlot(v[,i],colnames(v)[i],target)
      
    }else if(sapply(v[colnames(v)[i] ], class)=='factor'){
      cat(paste('Categorical', sapply(v[colnames(v)[i] ], class)))
      cat("\n")
      cat(paste('Unique: ', count(unique(v[colnames(v)[i] ]))))
      cat("\n")
      cat(paste("NA values: ", percent(sum(is.na(v[colnames(v)[i] ]))/nrow(v))))
      cat("\n")
      if(sum(is.na(v[colnames(v)[i] ]))/nrow(v)>0){
        cat('Consider converting NA to 0')
        colnames(v)[i] = 'None'
        cat("\n")
      }else{
        cat('Variable OK')
        cat("\n")
      }
      catPlot(v[,i],colnames(v)[i])
    }
  }
   sink()
   file.show("suggest.txt")
   closeAllConnections()
  return (v)
}