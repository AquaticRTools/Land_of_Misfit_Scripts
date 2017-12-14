# ------------------------
# R basics
# March 14, 2017
# R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: i686-pc-linux-gnu (32-bit)
# Jason R Hill - used code examples from Lou Reynolds and Emma Jones
# to create code for my wife's science and math classes.
# She called me a huge nerd - 'whatever'!
# Shows students how to make simple graphs and 
# How to calculate mean, median, and mode from made up test results
# Then I show them get the SOL "passrate" data from
# Department of Educaton and how to filter using
# plyr and dplyr and finally they make
# nicer graphs using ggplot2
# 
# -----------------------

###MeanMedianMode fun
###Boxplot fun
###Jitterplot fun

### Install packages
#install.packages('plyr')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('modeest')

# Loading packages
library(plyr)
library(dplyr)
library(ggplot2)
# ALWAYS LOAD plyr BEFORE dplyr

### Load data
# As .csv 
teacher <- read.csv('data/mathscores.csv') # because we created a project (set working directory)
teacher2 <- read.csv('data/mathscoresbox.csv') # because we created a project (set working directory)


###View data 
names(teacher) ## all the columns names
head(teacher) ## first 10 rows of data
tail(teacher) ## last 10 rows of data


### base R graph
qplot(data=teacher,x=StudentID,y=CrystalSpring,geom='boxplot') #boxplot, not so good of plot
qplot(data=teacher2,x=School,y=ScienceScore,geom='boxplot') #boxplot, better

# looks great, but can do even more 
# For more control use ggplot
p <- ggplot(data=teacher2,aes(x=School,y=ScienceScore))
p
p+ geom_point()
p+ geom_point(shape=1)
p+ geom_point(aes(color=School))
###plots all science scores as points and colors points by school


###make a fancy pretty boxplot of all data
p1 <- ggplot(data=teacher2,aes(x=School,y=ScienceScore)) +
  geom_boxplot(aes(color=School)) +
  ggtitle('Science Scores by School') +
  labs(x='School',y='ScienceScore') +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        legend.position="right")
p1

#Now lets add some addition data to this graph. 
#Jitter plot puts all the data over boxplot
p1+geom_jitter(aes(color=School))



mean <- mean(teacher$CrystalSpring)
mean
mean <- mean(teacher$Wasena)
mean
median <- median(teacher$CrystalSpring)
median
median <- median(teacher$Wasena)
median
###Mode not standard in base R
#Example function

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode1 <- Mode(teacher$CrystalSpring)
Mode1
Mode2 <- Mode(teacher$Wasena)
Mode2
Mode3 <- Mode(teacher2$ScienceScore)
Mode3
###Mode looks wonky due to low n

library(modeest)
## Most frequent value (mode)
Mode4 <-mfv(teacher$CrystalSpring, method="discrete")
Mode4
Mode5 <-mfv(teacher$Wasena, method="discrete")
Mode5
Mode6 <-mfv(teacher2$ScienceScore, method="discrete")
Mode6
plot(Mode6)

###Average put in dataframe
###Use dpylr
### mutate, add columns
teacher <- select(teacher,CrystalSpring,Wasena)%>%
  mutate(CrystalMean=mean(CrystalSpring))%>%
  mutate(WasenaMean=mean(Wasena))%>%
  mutate(CrystalMedian=median(CrystalSpring))%>%
  mutate(WasenaMedian=median(Wasena))

###Use Summerize to put into one line in another table
teacherStat <- summarise(teacher,mean(Wasena),mean(CrystalSpring),
                         median(Wasena),median(CrystalSpring))
  
  

### Load Science SOL Passrate data
# As .csv 
state <- read.csv('data/science_division_subject.csv') #science pass rates for the last 3 years

#check strings
str(state)
### state provided the pass rate as factor data not as an integer (numberic)
### Ha, they don't want you cant to do stats on their data?!?
state$X20132014PassRate <- as.numeric(as.character(state$X20132014PassRate))
state$X20142015PassRate <- as.numeric(as.character(state$X20142015PassRate))
state$X20152016PassRate <- as.numeric(as.character(state$X20152016PassRate))
str(state) ##now in a number format and can do math

###Use dpylr to filter science and all students
###Filter call
## Pipes (%>%), these are streamlining operations allowing you to flow from 
## one command to the next
stateScience <- select(state,DivisionName:X20152016PassRate) %>% # loses Level,DivNum,DivisionName
  filter(Subject=="Science" & Subgroup=="All Students")  # take only sciene and all students
  

###Use Summerize to put into one line in another table
StateStat <- summarise(stateScience,mean(X20132014PassRate),mean(X20142015PassRate),mean(X20152016PassRate),
                         median(X20132014PassRate),median(X20142015PassRate),median(X20152016PassRate))

## Most frequent value (mode), works better with a big dataset
Mode7 <-mfv(stateScience$X20132014PassRate, method="discrete")
Mode7
Mode8 <-mfv(stateScience$X20142015PassRate, method="discrete")
Mode8
Mode8 <-mfv(stateScience$X20152016PassRate, method="discrete")
Mode8

###Use GGplot to make bar plots
SciencePass <- ggplot(stateScience,aes(DivisionName,X20152016PassRate))+
  geom_bar(stat='identity', fill="steelblue")+
  labs(title="Science Passrate 2015-2016 By District", 
       x="District", y = "PassRate")

###wow hard to read even when you flip x axis, neat to see it all though
SciencePass +  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
            axis.text.y=element_text(angle = 90, hjust = 1)) 


## maybe just get first 30
stateScience2 <- slice(stateScience, 1:30) #just gets rows 1 to 30

###Use GGplot to make bar plots
SciencePass2 <- ggplot(stateScience2,aes(DivisionName,X20152016PassRate))+
  geom_bar(stat='identity', fill="steelblue")+
  labs(title="Science Passrate 2015-2016 By District", 
       x="District", y = "PassRate")

###wow hard to read even when you flip x axis, neat to see it all though
SciencePass2 +  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                     axis.text.y=element_text(angle = 90, hjust = 1)) 


###Use GGplot to make bar plots with multi years
SciencePass3 <- ggplot(stateScience2,aes(DivisionName,X20142015PassRate))+geom_bar(stat='identity') +
  labs(title="Science Passrate 2014-2015 and 2015-2016 By District", x="District", y = "PassRate") +
  geom_bar(data=stateScience2,aes(DivisionName,X20152016PassRate, color='Passrate 2015-2016'), stat='identity') +
  geom_bar(data=stateScience2,aes(DivisionName,X20142015PassRate, color='Passrate 2014-2015'), stat='identity') +
  scale_color_manual(values=c("Passrate 2015-2016"="green", "Passrate 2014-2015"="blue" ), name= "Science PassRate")

###wow hard to read even when you flip x axis, neat to see it all though
SciencePass3 +  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                      axis.text.y=element_text(angle = 90, hjust = 1)) 

 

### maybe just get data of local interest
### group_by, helper data management function for data organization
stateScience3 <- stateScience[2:2,1:6]
stateScience4 <- stateScience[11:11,1:6]
stateScience5 <- stateScience[13:13,1:6]
stateScience6 <- stateScience[19:19,1:6]
stateScience7 <- stateScience[31:31,1:6]
stateScience7 <- stateScience[41:41,1:6]
stateScience8 <- stateScience[43:43,1:6]
stateScience9 <- stateScience[80:80,1:6]
stateScience10 <- stateScience[105:105,1:6]
stateScience11 <- stateScience[106:106,1:6]
stateScience12 <- stateScience[110:110,1:6]
local <- rbind(stateScience3,stateScience4,stateScience5,stateScience6,stateScience7,
               stateScience8,stateScience9,stateScience10,stateScience11,stateScience12)

###Use GGplot to make bar plotsof local data
SciencePass4 <- ggplot(local,aes(DivisionName,X20152016PassRate))+
  geom_bar(stat='identity', fill="steelblue")+
  labs(title="Science Passrate 2015-2016 By District", 
       x="District", y = "PassRate")

###now you can see all the local school districts
###also - you can roanoke city has the lowest pass rate, sorry dear!
SciencePass4 +  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                      axis.text.y=element_text(angle = 90, hjust = 1)) 



###Use dpylr to filter all subjects and all students
###Filter call
## Pipes (%>%), these are streamlining operations allowing you to flow from 
## one command to the next
stateAll <- select(state,Subject:X20152016PassRate) %>% # loses Level,DivNum,DivisionName
  filter(Subgroup=="All Students")  # takes all students subgroup


###make a another fancy pretty boxplot of all data in ggplot
pAll <- ggplot(data=stateAll,aes(x=Subject,y=X20152016PassRate)) +
  geom_boxplot(aes(color=Subject)) +
  ggtitle('Passrate by Subject') +
  labs(x='Subject',y='Passrate') +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        legend.position="right") +
        geom_hline(yintercept=60,linetype = "longdash") ##dash line of passing cutoff
pAll

#Now lets add some addition data to this graph. 
#Jitter plot puts all the scores by data over boxplot
pAll+geom_jitter(aes(color=Subject))+theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                                             axis.text.y=element_text(angle = 90, hjust = 1))

###drops jitter you hate it
pAll +  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
              axis.text.y=element_text(angle = 90, hjust = 1)) 




