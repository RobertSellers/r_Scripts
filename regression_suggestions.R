suppressWarnings(require(moments))
suppressWarnings(require(dplyr))
suppressWarnings(require(e1071))

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

intPlot <-function(v,name,target){
  plot(density(na.omit(v)), main=paste(name,"(Density)"))
  polygon(density(na.omit(v)), col="red", border="blue")
  plot(target, v, pch=21,  main=paste(name," vs Response"))
  qqnorm(v,main=paste(name,"Q-Q Plot"))
}

catPlot <-function(v,name,target){
  plot(v,main=name)
  box(lty = '1373', col = 'black')
}

predictorPlots<-function (v,target){
  par(mfrow=c(2,4))
  i<-0
  for(j in names(v)){
    i<-i+1
    if(sapply(v[colnames(v)[i] ], class)=='integer'){
      intPlot(v[,i],colnames(v)[i],target)
    }
    # else if(sapply(v[colnames(v)[i] ], class)=='factor'){
      # DISABLED
    #  if(sum(is.na(v[colnames(v)[i] ]))/nrow(v)>0){
    #    catPlot(v[,i],colnames(v)[i],target)
    #  }

    # }
  }
}
plotPCA<-function(train){
  suppressWarnings(require(ggplot2))
  par(mar = rep(2, 4))
  theta <- seq(0,2*pi,length.out = 100)
  circle <- data.frame(x = cos(theta), y = sin(theta))
  p <- ggplot(circle,aes(x,y)) + geom_path()
  loadings <- data.frame(data.pca$rotation, 
  .names = row.names(data.pca$rotation))
  p + geom_text(data=loadings, 
  mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
  
}


suggest<-function (v,target){
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
    }
  }
   sink()
   file.show("suggest.txt")
   closeAllConnections()
  return (v)
}