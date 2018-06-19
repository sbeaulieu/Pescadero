library(lubridate)
library(PescStats)
ISS<-read.csv("temp_log_002.csv")
ISS$time<-ymd_hms(ISS$time)
start<-ymd_hms("2017-10-31T20:53:26.428Z")
end<-ymd_hms(start)+minutes(10)
ISS_2min<-subset(ISS, time>start & time<end)
writeLines(paste("Temperature Data from",start,"to",end))
sum_stats(ISS_2min$temperature)
barplot(ISS_2min$temperature, xlab=paste("Temps for",start,"to",end))


#added sum_stats() and SEM(), which sum_stats() depends on, to PescStats
#fixed subsetting so that it actually subsets, instead of grabbing the whole data set
#note: this script exists as a function in PescStats, as TempStats.R
