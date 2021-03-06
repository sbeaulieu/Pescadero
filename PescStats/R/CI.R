#'Confidence Interval Function
#'
#'This function outputs the confidence interval for a data set x with a critical value of t*, appearing as 't' in this function. The critical
#'value may be found using the appropriate chart and desired percent confidence interval; it is NOT calculated here. The result is printed as
#'characters, not numeric values. It is dependent on the SEM() function, which is also included in this package. This function is written to 
#'exclude NA values.
#'
#'@usage  CI(x,z)
#'@param x the data set for which the summary statistics is desired
#'@param t the critical value z* for the desired confidence interval
#'@details  The confidence interval is calculated using the sample mean (mean(x)), the critical interval (z), and the Standard Error of the 
#'Mean. SEM() is defined in its own function as the standard deviation of sample x divided by the number of samples in x. 
#'          Formula: Confidence Interval = mean(x) \u00B1 t*(sd(x)/sqrt(n)), where n is the number of units in x (ie, length(x)), so
#'                   Confidence Interval = mean(x) \u00B1 t*SEM(x)
#'@examples For a confidence interval of 95% for columnName data from FileName.csv
#'          file<-read.csv("FileName.csv")
#'          CI(file$columnName,1.960)
#'Confidence Interval: L1 to L2
#'          For a 95% confidence interval for the speeds of built-in data set cars
#'Confidence Interval: 13.9 to 16.9 #the result, as printed in the console
#'          
#'@export

CI<-function(x,t){
  decimal<-strsplit(as.character(x),"\\.")[[1]][2]
  if(is.na(decimal))
    digits <- 1
  else
    digits<-nchar(decimal+1)
  L1<-round(mean(x, na.rm=T)-t*SEM(x), digits=digits)
  L2<-round(mean(x, na.rm=T)+t*SEM(x),digits=digits)
  interval<-paste("Confidence Interval:",L1,"to",L2)
  writeLines(interval)
}
