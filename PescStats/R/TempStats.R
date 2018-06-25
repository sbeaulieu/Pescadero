#'Pescadero Temperature Data Analysis Function
#'
#'This function depends on the package 'lubridate', as well as the functions sum_stats() and SEM().
#'Lubridate is not included in this package, PescStats, but sum_stats() and SEM() are. This function 
#'prints the mean, median, range, variance, standard deviation, and standard error of the mean for data 
#'in the "temperature" column of a .csv file for a given time range, starting at a given time. It also 
#'generates a bar plot of the temperatures for that range.
#'@usage  TempStats(x,t,d,type=c(barplot,plot),title=" ")
#'@param x file name of the csv containing the desired temperature data (eg: "ISS_001_log.csv")
#'@param t the timestamp of the desired start time, including the year, month, day, hour, minute, and second. (eg. 2017-10-31T19:00:12.656Z)
#'@param duration the duration, in minutes, of the data one wishes to analyze
#'@param title the desired title of the barplot, in quotes; if NULL, barplot will be untitled.
#'@param type the type of the graph output, either "barplot" or "scatterplot". Default is a barplot.
#'@examples
#'          TempStats("temp_log_001.csv","2017-10-31 14:48:00",10,"ISS1 Bullseye")
#'Temperature Data from 2017-10-31 14:48:00 to 2017-10-31 14:58:00
#'Summary Statistics:
#' mean = 40.627
#' median = 40.48
#' range = 16.78
#' variance = 12.122
#' standard deviation = 3.482
#' Standard Error of the Mean = 0.176
#' 
#'Also creates a barplot of 10 minutes of temperature data from temp_log_001.csv, starting at 2017-10-31 14:48:00, titled "ISS1 Bullseye"
#'  
#'          TempStats("temp_log_002.csv","2017-10-31T20:53:26.428Z",15,"ISS2 Away")
#'Temperature Data from 2017-10-31 20:53:26 to 2017-10-31 21:08:26
#'Summary Statistics:
#'  mean = 9.511
#'  median = 9.27
#'  range = 10.4
#'  variance = 10.987
#'  standard deviation = 3.315
#'  Standard Error of the Mean = 0.135
#'
#'Also creates a scatterplot of 15 minutes of temperature data from temp_log_002.csv, starting at 2017-10-31T20:53:26.428Z, titled "ISS2 Away"  
#'
#'@export

TempStats<-function(x,t,d,title,type){
  ISS<-read.csv(x)
  ISS$time<-ymd_hms(ISS$time)
  start<-ymd_hms(t)
  end<-ymd_hms(start)+minutes(d)
  ISS_duration<-subset(ISS, time>start & time<end)
  writeLines(paste("Temperature Data from",start,"to",end))
  sum_stats(ISS_duration$temperature)
 
  if (missing(title))
  title<-NULL
 else
   title=title
  
 if (missing(type))
  barplot(ISS_duration$temperature, main=title, xlab=paste("Temps for",start,"to",end),ylab="Temperature (C)")  
  else if (type == "barplot")
   barplot(ISS_duration$temperature, main=title, xlab=paste("Temps for",start,"to",end),ylab="Temperature (C)")
 else if(type == "scatterplot")
   plot(ISS_duration$temperature, main=title, xlab=paste("Temps for",start,"to",end),ylab="Temperature (C)",pch=20)
 else 
   print ("Error generating graph: please specify type= ''barplot'' or ''plot''")
}