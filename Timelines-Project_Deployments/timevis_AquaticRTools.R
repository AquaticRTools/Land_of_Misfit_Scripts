# ----------------------------------------------------------------------------------
# The ultimate purpose of this code is to create visual timelines that display
# the time period continuous data loggers (or field meters) have been
# deployed at each site. This code walks through four examples to familiarize
# the user with the package timevis(). The code can be modified in a number of ways,
# including showing the reverse, the sites associated with each logger.
# It could also be easily modified for other project management applications.
# 
# There are four examples in this script, each with more complexity
# (data formatting) required.  All dataframes are provided in the code.
# 1: Example from the timevis() help.
# 2: Timeline from a dataframe containing logger name, start and
# end times, with fields already named to match timevis() argument requirements
# 3: Timmeline from a dataframe containing only start (field visit) times and
# different column (field) names than timevis arguments.  Have to determine end time.
# 4: Same data as in # 3, but displaying the opposite, sites associated with each logger.
# 
# For more information on timevis(), see:
# https://github.com/daattali/timevis#slightly-more-advanced-examples
# 
# Timevis() is also desgined for use in shiny apps and can be interactive.
# 
# A red line automatically appears that marks the current time.  I don't know
# how to delete it yet.
# 
# Leah Ettema
# ettema.leah@epa.gov
#
#------------------------------------------------------------------------

library(timevis)
library(dplyr)
library("lubridate")

##1. example from ?timevis()####______________________________________________________________________
#timevis() takes dataframes, with specific formatting, as arguments
timevis(data = data.frame(
                start = c(Sys.Date(), Sys.Date(), Sys.Date() + 1, Sys.Date() + 2),
                content = c("one", "two", "three", "four"),
                group = c(1, 2, 1, 2)),
              groups = data.frame(id = 1:2, content = c("G1", "G2"))
        )

####2. Example of timevis() with dataframe that already contains end-times#####________________________________________________________
#If times are not formatted "1988-11-22" or "1988-11-22 16:30:00", 
#use the lubridate package or base R functions to convert format

test<-data.frame(start= c("2017-11-22 16:30:00", "2018-03-24 09:02:00", "2018-06-27 12:01:00", 
                          "2017-11-22 14:48:00", "2018-03-24 10:15:00", "2017-11-25 13:36:00"),
                 content=c("logger 1", "logger 1", "logger 4", "logger 2", "logger 2", "logger 3"),
                 groups=c("TM01", "TM01", "TM01" , "TM02", "TM02", "TM03"),
                 end=c("2018-03-24 09:00:00", "2018-06-27 08:59:00", "2018-11-25 13:36:00", 
                       "2018-03-24 10:10:00", "2018-11-25 16:36:00", "2018-02-25 13:36:00"))

#Create dataframe to specify group levels
test$group <- as.numeric(as.factor(test$groups)) #group id

groups<-test %>%
  select(content=groups, id=group) 
groups<-subset(groups, !duplicated(groups$id)) #select only unique group ids

#Filter data to match timevis() Arguments
test<-select(test, -groups)

test$type<-"range" #specify types of plot (see timevis() help)

timevis(data = test, 
        groups = groups,
        options = list(stack = FALSE)) 
        # found stack option here: https://stackoverflow.com/questions/45235338/subgroups-in-r-timevis

####3. Example with only field visit times specified, not end-times, ####_____________________________________________________
#      and column names need to be changed to match timevis() requirements

#If times are not formatted "1988-11-22" or "1988-11-22 16:30:00", 
#use the lubridate package or base R functions to convert format

inputdata<-data.frame(time= c("2017-11-22 16:30:00", "2018-03-24 09:02:00", "2018-05-01 08:01:00", "2018-06-27 12:01:00",
                                 "2017-11-22 14:48:00", "2018-03-24 10:15:00", "2017-11-25 13:36:00", "2018-04-25 15:36:00"),
                        logger=c("logger 1", "logger 1", "logger 4", "logger 4", "logger 2", "logger 2", "logger 3", "logger 5"),
                        site=c("TM01", "TM01", "TM01" ,"TM01", "TM02", "TM02", "TM03", "TM03"), stringsAsFactors=FALSE)

#Order data by time and then by site.  Needed for creating end time values
inputdata<-inputdata[order(inputdata$time),]
inputdata<-inputdata[order(inputdata$site),]

#Make variable names match arguments in timevis function
tldata<-select(inputdata, start=time, content=logger, groups=site) #tl = timeline, timelinedata

rows<-nrow(tldata) #number of rows in dataframe

###Determine end time for each logger deployment####

   #Code below copies and pastes the site name (groups) and start time to new columns, starting with the second element, 
   #(So, subsequent field visit times (end deployemnet times) are pasted on same row as when the logger was first deployed). 
   #Tf the site of the end deployment time does not match the site at the beginning deployment time, the logger from 
   #the field visit of interest is still deployed or has already been pulled, and the end time is changed to NA.

#time vector
end<-tldata$start #creating a vector the length of data frame, contents don't matter
end[1:(rows-1)]<-tldata$start[2:rows] #fill end times with start times starting at row 2 (copying times)

#site vector
endsite<-tldata$groups #creating a vector the length of data frame, contents don't matter
endsite[1:(rows-1)]<-as.character(tldata$groups[2:rows]) #copying sites

#pasting the vectors into the dataframe
tldata$end<-end
tldata$endsite<-endsite 

#If site names do not match, change end time to NA
i=1
for(i in 1:(rows-1))
  {
  if(tldata$endsite[i]!=tldata$groups[i])
  { tldata$end[i]=NA}
}

#Notes: 
  # The start and end time for logger 5 is the same, so it will appear as a thick blue line.
  # We are assuming loggers are removed if there is a new field visit with a new logger.

###Now all field visits/logger deployments have end times, if applicable

####Format Arguments to  match Timevis requirements####

#if there are groups, timevis() needs those in a separate dataframe.  The initial dataframe
#must contain only variables specified in the timevis() function arguments

#creating the dataframe to specify groups
tldata$group <- as.numeric(as.factor(tldata$groups))

groupsdf<-tldata %>%
  select(content=groups, id=group) 
groupsdf<-subset(groupsdf, !duplicated(groupsdf$id))

#filtering to match timevis() arguments
#content is what is displayed on the timeline
#the names of the groups are in the groupsdf (the id field in the data links the groupsdf)
tldata<-tldata %>%
  select(-groups) %>% #
  filter(!(is.na(end))) #filter out values without an end time

tldata$type<-"range" 

timevis(data = tldata, 
        groups = groupsdf,
        options = list(stack = FALSE))
#found options here: https://stackoverflow.com/questions/45235338/subgroups-in-r-timevis

####4. Example displaying timelines of site by loggers####
#change code when making the group dataframe (line 191)
inputdata<-data.frame(time= c("2017-11-22 16:30:00", "2018-03-24 09:02:00", "2018-05-01 08:01:00", "2018-06-27 12:01:00",
                              "2017-11-22 14:48:00", "2018-03-24 10:15:00", "2017-11-25 13:36:00", "2018-04-25 15:36:00"),
                      logger=c("logger 1", "logger 1", "logger 4", "logger 4", "logger 2", "logger 2", "logger 3", "logger 5"),
                      site=c("TM01", "TM01", "TM01" ,"TM01", "TM02", "TM02", "TM03", "TM03"), stringsAsFactors=FALSE)

#Order data by time and then by site.  Needed for creating end time values
inputdata<-inputdata[order(inputdata$time),]
inputdata<-inputdata[order(inputdata$site),]

#Make variable names match arguments in timevis function
tldata<-select(inputdata, start=time, content=logger, groups=site) #tl = timeline, timelinedata

rows<-nrow(tldata) #number of rows in dataframe

###Determine end time for each logger deployment####

#time vector
end<-tldata$start #creating a vector the length of data frame, contents don't matter
end[1:(rows-1)]<-tldata$start[2:rows] #fill end times with start times starting at row 2 (copying times)

#site vector
endsite<-tldata$groups #creating a vector the length of data frame, contents don't matter
endsite[1:(rows-1)]<-as.character(tldata$groups[2:rows]) #copying sites

#pasting the vectors into the dataframe
tldata$end<-end
tldata$endsite<-endsite 

#If site names do not match, change end time to NA
i=1
for(i in 1:(rows-1))
{
  if(tldata$endsite[i]!=tldata$groups[i])
  { tldata$end[i]=NA}
}

#creating the dataframe to specify groups
#switch groups to content and content to groups (site to logger)
tldata$group <- as.numeric(as.factor(tldata$content))

groupsdf<-tldata %>%
  select(content=content, id=group) 

groupsdf<-subset(groupsdf, !duplicated(groupsdf$id))

#filtering to match timevis() arguments
#content is what is displayed on the timeline
#the names of the groups are in the groupsdf (the id field in the data links the groupsdf)
tldata<-tldata %>%
  select(-content) %>% # drop content and then make groups the content field in the next line
  select(start, end, content=groups, group, endsite) %>%
  filter(!(is.na(end))) #filter out values without an end time

tldata$type<-"range" 

timevis(data = tldata, 
        groups = groupsdf,
        options = list(stack = FALSE))
