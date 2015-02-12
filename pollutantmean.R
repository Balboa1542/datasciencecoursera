pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  final<-c()

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
      if(pollutant == "nitrate"){
          sub<-subset(csv, !is.na(nitrate), select = pollutant)
      }
      else{
          sub<-subset(csv, !is.na(sulfate), select = pollutant)
      }
      
      final<-rbind(final, sub)
      }
  
  result<-apply(final,2,mean)
  
  return(result)
  
}
