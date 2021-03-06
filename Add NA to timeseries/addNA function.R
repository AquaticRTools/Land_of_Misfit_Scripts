addNA<-function(data, start, end, by='1 hour'){

#data= dataframe with a 'site' column and 'time' column (or modify code below)
#start=POSIXct start time
#end=POSIXct end time
#by=time stamp time difference

  #generating data frame with all dates between start and end time
  full <- data.frame(time=seq(start, end, by=by))
  site<-data$site[1]

  #joining data and empty data frame
  alldata<-full_join(data, full, by="time")
  alldata$site<-site #assign missing values a siteID

  #put all values in chronological order
  #dataframe[row,column] is another way to get data
  alldata<-alldata[order(alldata$time),]

return(alldata)
}
