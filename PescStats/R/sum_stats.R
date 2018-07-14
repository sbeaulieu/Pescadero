#'Summary Statistics Function
#'
#'This function calculates the mean, median, range, variance, and standard deviation, and prints them as an appropriately labelled parcel of 
#'text. It also adheres to the rounding conventions (to one greater decimal than measured) established in class. Additionally, NA values are 
#'removed within the function.
#'@usage  sum_stats(x)
#'@param x the data set for which the summary statistics is desired
#'@examples x<- c(0.1,0.2,0.3,0.4) #data is measured to one decimal place
#'          sum_stats(x)
#'Summary Statistics:
#'mean = 0.25 #results are given to two decimal places (decimals measured to + 1)
#'median = 0.25
#'range = 0.3 #R does not typically include terminal 0s in decimals reported from calculations
#'variance = 0.02
#'standard deviation = 0.13
#'@export

sum_stats<-function(x){
  decimal<-strsplit(as.character(x),"\\.")[[1]][2]
  if(is.na(decimal))
    digits <- 1
  else
    digits<-nchar(decimal)+1
  min<-paste("minimum =", round(min(x, na.rm=T), digits=digits))
  max<-paste("maximum =", round(max(x, na.rm=T), digits=digits))
  med<-paste("median =", round(median(x, na.rm=T), digits=digits))
  mean<-paste("mean =", round(mean(x, na.rm=T), digits=digits))
  range<-paste("range =", round(max(x, na.rm=T)-min(x, na.rm=T), digits=digits))
  var<-paste("variance =", round(var(x, na.rm=T), digits=digits))
  sd<-paste("standard deviation =", round(sd(x, na.rm=T), digits=digits))
  SEM<-paste("Standard Error of the Mean =",round(SEM(x),digits=digits))
  sum_stat<-c("Summary Statistics:",max,min,med,mean,range,var,sd,SEM,"")
  writeLines(sum_stat)
}

