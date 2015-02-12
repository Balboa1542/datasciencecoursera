complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  nobs<-c()
  
  for(i in id){
    if(i<10){
      csv<-read.csv(paste0(getwd(),"/",directory,"/","00",i,".csv"))   
    }
    else if(i<100){
      csv<-read.csv(paste0(getwd(),"/",directory,"/","0",i,".csv"))  
    }
    else{
      csv<-read.csv(paste0(getwd(),"/",directory,"/",i,".csv"))
    }
    
    sub<-subset(csv, !is.na(nitrate) & !is.na(sulfate), ID)
    
    nobs<-c(nobs, nrow(sub))
        
  }
  
  return(data.frame(cbind(id,nobs)))
  
}
