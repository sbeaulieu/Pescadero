#'Standard Error of the Mean
#'
#'This function calculates the standard error of the mean for a given data set x. It outputs a numeric value for the standard deviation of x 
#'over the root of the number of values in x. It does not include a rounding function, but does remove NA values.
#'@usage  SEM(x)
#'@param x the data set for which the summary statistics is desired
#'@details As mentioned above, the Standard Error of the Mean is calculated from the standard deviation of x, and the square root of n, the 
#'number of measurements in x. The formula used is as follows:
#'          SEM= sd(x)/sqrt(n)
#'@examples Calculating the Standard Error of the Mean for columnName in NameOfDataFile data set
#'          file <-read.csv("NameOfDataFile.csv")
#'          SEM(file$columName)
#'          #value of SEM (numeric)
#'          
#'          Calculating the Standard Error of the Mean for speed in the built-in data set 'cars'.
#'          SEM(cars$speed)
#'          0.7477858 #Standard error of the mean of speed data in the built-in data set cars
#'@export


SEM<-function(x){
  x<-na.omit(x)
  sd(x)/sqrt(length(x))
}
