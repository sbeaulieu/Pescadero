#'Pescadero Temperature Data Analysis Function
#'
#'This function depends on the package 'lubridate', as well as the functions sum_stats() and SEM().
#'Lubridate is not included in this package, PescStats, but sum_stats() and SEM() are. This function 
#'prints the mean, median, range, variance, standard deviation, and standard error of the mean for data 
#'in the "temperature" column of a .csv file for a given time range, starting at a given time. It also 
#'generates a bar plot of the temperatures for that range.
#'@usage  TempStats(x,t,d)
#'@param x file name of the csv containing the desired temperature data (eg: "ISS_001_log.csv")
#'@param t the timestamp of the desired start time, including the year, month, day, hour, minute, and second. (eg. 2017-10-31T19:00:12.656Z)
#'@param d the duration, in minutes, of the data one wishes to analyze
#'@examples
#'          TempStats("ISS_001_log.csv","2017-10-31 14:48:00",10)
#'Temperature Data from 2017-10-31 14:48:00 to 2017-10-31 14:58:00
#'Summary Statistics:
#' mean = 40.627
#' median = 40.48
#' range = 16.78
#' variance = 12.122
#' standard deviation = 3.482
#' Standard Error of the Mean = 0.176
#'  
#'          TempStats("temp_log_002.csv","2017-10-31T20:53:26.428Z",15)
#'Temperature Data from 2017-10-31 20:53:26 to 2017-10-31 21:08:26
#'Summary Statistics:
#'  mean = 9.511
#'  median = 9.27
#'  range = 10.4
#'  variance = 10.987
#'  standard deviation = 3.315
#'  Standard Error of the Mean = 0.135
#'@export

TempStats<-function(x,t,d){
  ISS<-read.csv(x)
  ISS$time<-ymd_hms(ISS$time)
  start<-ymd_hms(t)
  end<-ymd_hms(start)+minutes(d)
  ISS_duration<-subset(ISS, time>start & time<end)
  writeLines(paste("Temperature Data from",start,"to",end))
  sum_stats(ISS_duration$temperature)
  barplot(ISS_duration$temperature, xlab=paste("Temps for",start,"to",end))
}
