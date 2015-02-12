corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  final<-c()
  
  for(i in 1:332){
    if(i<10){
      csv<-read.csv(paste0(getwd(),"/",directory,"/","00",i,".csv"))   
    }
    else if(i<100){
      csv<-read.csv(paste0(getwd(),"/",directory,"/","0",i,".csv"))  
    }
    else{
      csv<-read.csv(paste0(getwd(),"/",directory,"/",i,".csv"))
    }
    
    sub<-subset(csv, !is.na(nitrate) & !is.na(sulfate), nitrate:sulfate)
    
    if(nrow(sub)>threshold){
      final<-c(final,cor(sub$sulfate, sub$nitrate))
    }
    
  }
  
  return(final)

}
