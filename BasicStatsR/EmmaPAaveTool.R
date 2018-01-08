# ProbMon-Ambient Field Data Analysis Tool
# This tool performs basic statistics on monthly field data collected from ProbMon stations across Virginia
# Created by Emma Jones
# Last Updated by JRH: 1/04/2018
###Emma Code Orginal is at end of Sheet
###JRH modified to Average Target Stress Data

library(readxl)
library(xlsx)
library(plyr)
library(dplyr)
library(magrittr)
library(reshape2)
library(tidyr)
# Set working directory

setwd('C:/Users/ktq89598/Documents/EmmaPAaveTool')

# Load data
FieldDataStress <- read_excel('data/LucyDataWaterChem.xlsx', sheet='Field')


# Count data for each parameter
FieldDataStresswide <- FieldDataStress %>%
  select(StationID,PH:SPCOND)%>% # Drop excess columns 
  gather(variable,value,PH:SPCOND)%>% # Convert to long format for easier math
  group_by(StationID,variable)%>% # Must group stations together to get correct n
  summarise(n=sum(!is.na(value)))%>% # Count number of measures taken per variable
  spread(variable,n) # Convert to wide format for easier viewing


# Simple stats
FieldDataStressStat <- ddply(FieldDataStress,c('StationID'),summarize
                     ,DOmean=mean(DO,na.rm=T),DOmedian=median(DO,na.rm=T)
                     ,DOmin=min(DO,na.rm=T),DOmax=max(DO,na.rm=T)
                     ,pHmean=mean(PH,na.rm=T),pHmedian=median(PH,na.rm=T)
                     ,pHmin=min(PH,na.rm=T),pHmax=max(PH,na.rm=T)
                     ,SPcondmean=mean(SPCOND,na.rm=T),SPcondmedian=median(SPCOND,na.rm=T)
                     ,SPcondmin=min(SPCOND,na.rm=T),SPcondmax=max(SPCOND,na.rm=T))

# Merge n to stats results dataframe
FieldDataStressResults <- merge(FieldDataStresswide, FieldDataStressStat, by='StationID')

# Output data to .xlsx
write.xlsx(FieldDataStressResults,'C:/Users/ktq89598/Documents/EmmaPAaveTool/data/LucyDataWaterChemField.xlsx'
           ,sheetName='FieldDataStressResults', col.names=T)




###################################################################################
###Emma Original Code
# Count data for each parameter
PAdatawide2013 <- PAdata2013 %>%
  select(StationID,DoProbe:EC)%>% # Drop excess columns 
  gather(variable,value,DoProbe:EC)%>% # Convert to long format for easier math
  group_by(StationID,variable)%>% # Must group stations together to get correct n
  summarise(n=sum(!is.na(value)))%>% # Count number of measures taken per variable
  spread(variable,n) # Convert to wide format for easier viewing

# Simple stats
PA2013calcs <- ddply(PAdata2013,c('StationID'),summarize
                     ,Tmean=mean(TempCelcius,na.rm=T),Tmedian=median(TempCelcius,na.rm=T)
                     ,Tmin=min(TempCelcius,na.rm=T),Tmax=max(TempCelcius,na.rm=T)
                     ,DOmean=mean(DoProbe,na.rm=T),DOmedian=median(DoProbe,na.rm=T)
                     ,DOmin=min(DoProbe,na.rm=T),DOmax=max(DoProbe,na.rm=T)
                     ,pHmean=mean(FieldPh,na.rm=T),pHmedian=median(FieldPh,na.rm=T)
                     ,pHmin=min(FieldPh,na.rm=T),pHmax=max(FieldPh,na.rm=T)
                     ,SPcondmean=mean(SpecificConductance,na.rm=T),SPcondmedian=median(SpecificConductance,na.rm=T)
                     ,SPcondmin=min(SpecificConductance,na.rm=T),SPcondmax=max(SpecificConductance,na.rm=T)
                     ,TNmean=mean(TN,na.rm=T),TNmedian=median(TN,na.rm=T)
                     ,TNmin=min(TN,na.rm=T),TNmax=max(TN,na.rm=T)
                     ,TPmean=mean(TP,na.rm=T),TPmedian=median(TP,na.rm=T)
                     ,TPmin=min(TP,na.rm=T),TPmax=max(TP,na.rm=T)
                     ,ECmean=mean(EC,na.rm=T),ECmedian=median(EC,na.rm=T)
                     ,ECmin=min(EC,na.rm=T),ECmax=max(EC,na.rm=T))

# Merge n to stats results dataframe
PA2013results <- merge(PA2013calcs, PAdatawide2013, by='StationID')

# Bacteria Exceedance Calculations, use piping istead of function to limit excess dataframes
ecoliResults2013 <- PAdata2013 %>%
  melt(id.vars=c('StationID'),measure.vars=c('EC')) %>%
  group_by(StationID) %>%
  na.omit() %>%
  mutate(n=n()) %>%
  filter(value>235) %>%
  summarise(eColiHits=n(),n=max(n)) %>%
  mutate(pctHit=(eColiHits/n)*100) %>%
  select(StationID,eColiHits,pctHit)

ecoliViolation2013 <- ecoliResults2013 %>%
  filter(pctHit>10.5) # dataframe of eColi > 10.5% samples per site



### Repeat for 2014
# Load data
PAdata2014 <- read_excel('2014_Probient_AllData_EVJ.xlsx', sheet='AllPAData')

# Count data for each parameter
PAdatawide2014 <- PAdata2014 %>%
  select(StationID,DoProbe:EC)%>% # Drop excess columns 
  gather(variable,value,DoProbe:EC)%>% # Convert to long format for easier math
  group_by(StationID,variable)%>% # Must group stations together to get correct n
  summarise(n=sum(!is.na(value)))%>% # Count number of measures taken per variable
  spread(variable,n) # Convert to wide format for easier viewing

# Simple stats
PA2014calcs <- ddply(PAdata2014,c('StationID'),summarize
                     ,Tmean=mean(TempCelcius,na.rm=T),Tmedian=median(TempCelcius,na.rm=T)
                     ,Tmin=min(TempCelcius,na.rm=T),Tmax=max(TempCelcius,na.rm=T)
                     ,DOmean=mean(DoProbe,na.rm=T),DOmedian=median(DoProbe,na.rm=T)
                     ,DOmin=min(DoProbe,na.rm=T),DOmax=max(DoProbe,na.rm=T)
                     ,pHmean=mean(FieldPh,na.rm=T),pHmedian=median(FieldPh,na.rm=T)
                     ,pHmin=min(FieldPh,na.rm=T),pHmax=max(FieldPh,na.rm=T)
                     ,SPcondmean=mean(SpecificConductance,na.rm=T),SPcondmedian=median(SpecificConductance,na.rm=T)
                     ,SPcondmin=min(SpecificConductance,na.rm=T),SPcondmax=max(SpecificConductance,na.rm=T)
                     ,TNmean=mean(TN,na.rm=T),TNmedian=median(TN,na.rm=T)
                     ,TNmin=min(TN,na.rm=T),TNmax=max(TN,na.rm=T)
                     ,TPmean=mean(TP,na.rm=T),TPmedian=median(TP,na.rm=T)
                     ,TPmin=min(TP,na.rm=T),TPmax=max(TP,na.rm=T)
                     ,ECmean=mean(EC,na.rm=T),ECmedian=median(EC,na.rm=T)
                     ,ECmin=min(EC,na.rm=T),ECmax=max(EC,na.rm=T))

# Merge n to stats results dataframe
PA2014results <- merge(PA2014calcs, PAdatawide2014, by='StationID')

# Bacteria Exceedance Calculations, use piping istead of function to limit excess dataframes
ecoliResults2014 <- PAdata2014 %>%
  melt(id.vars=c('StationID'),measure.vars=c('EC')) %>%
  group_by(StationID) %>%
  na.omit() %>%
  mutate(n=n()) %>%
  filter(value>235) %>%
  summarise(eColiHits=n(),n=max(n)) %>%
  mutate(pctHit=(eColiHits/n)*100) %>%
  select(StationID,eColiHits,pctHit)

ecoliViolation2014 <- ecoliResults2014 %>%
  filter(pctHit>10.5)

# Output data to .xlsx
write.xlsx(PA2013results,'C:/HardDriveBackup/R/ProbMon/ProbientCalculations/data/20132014results.xlsx'
           ,sheetName='PA2013results', col.names=T)
write.xlsx(ecoliResults2013,'C:/HardDriveBackup/R/ProbMon/ProbientCalculations/data/20132014results.xlsx'
           ,sheetName='ecoliResults2013', col.names=T, append=T)
write.xlsx(ecoliViolation2013,'C:/HardDriveBackup/R/ProbMon/ProbientCalculations/data/20132014results.xlsx'
           ,sheetName='ecoliViolation2013', col.names=T, append=T)
write.xlsx(PA2014results,'C:/HardDriveBackup/R/ProbMon/ProbientCalculations/data/20132014results.xlsx'
           ,sheetName='PA2014results', col.names=T, append=T)
write.xlsx(ecoliResults2014,'C:/HardDriveBackup/R/ProbMon/ProbientCalculations/data/20132014results.xlsx'
           ,sheetName='ecoliResults2014', col.names=T, append=T)
write.xlsx(ecoliViolation2014,'C:/HardDriveBackup/R/ProbMon/ProbientCalculations/data/20132014results.xlsx'
           ,sheetName='ecoliViolation2014', col.names=T, append=T)



# Quick comparison
summary(ecoliResults2013)
summary(ecoliResults2014)
summary(ecoliViolation2013)
summary(ecoliViolation2014)
boxplot(ecoliResults2013$eColiHits,ecoliResults2014$eColiHits) # n instantaneous hits
boxplot(ecoliResults2013$pctHit,ecoliResults2014$pctHit) # compare all violations yearly
boxplot(ecoliViolation2013$pctHit,ecoliViolation2014$pctHit) # compare >10.5% violation yearly

# eColi results similar between 2 years, main difference 2013= 25 sites of 37 monitored monthly
# violating bacteria standards (67.5%); 2014= 28 sites of 48 violating (58.3%)







#
##
### Older coding method, same results just more intermediate data frames created
##
#

# Data in long format to count missing data for each parameter
PAdatalong <- melt(PAdata2013, id.vars=c('StationID')
                   ,measure.vars=c('TempCelcius','DoProbe','FieldPh','SpecificConductance','TN'
                                   ,'TP','EC'))
# Count number of measures taken and missing data
PAdatalong1 <- ddply(PAdatalong, c('StationID','variable'),summarise,n=sum(!is.na(value))
                     ,naCount=sum(is.na(value)))
# Add n_ to each variable name
PAdatalong2 <- mutate(PAdatalong1,varCount=paste('n',variable,sep='_')) 
# Cast to wide format to show n measures for each parameter
PAdatawide <- dcast(PAdatalong2, StationID~varCount, value.var='n')
# Calculate basic stats on each parameter
PA2013calcs <- ddply(PAdata2013,c('StationID'),summarize
                     ,Tmean=mean(TempCelcius,na.rm=T),Tmedian=median(TempCelcius,na.rm=T)
                     ,Tmin=min(TempCelcius,na.rm=T),Tmax=max(TempCelcius,na.rm=T)
                     ,DOmean=mean(DoProbe,na.rm=T),DOmedian=median(DoProbe,na.rm=T)
                     ,DOmin=min(DoProbe,na.rm=T),DOmax=max(DoProbe,na.rm=T)
                     ,pHmean=mean(FieldPh,na.rm=T),pHmedian=median(FieldPh,na.rm=T)
                     ,pHmin=min(FieldPh,na.rm=T),pHmax=max(FieldPh,na.rm=T)
                     ,SPcondmean=mean(SpecificConductance,na.rm=T),SPcondmedian=median(SpecificConductance,na.rm=T)
                     ,SPcondmin=min(SpecificConductance,na.rm=T),SPcondmax=max(SpecificConductance,na.rm=T)
                     ,TNmean=mean(TN,na.rm=T),TNmedian=median(TN,na.rm=T)
                     ,TNmin=min(TN,na.rm=T),TNmax=max(TN,na.rm=T)
                     ,TPmean=mean(TP,na.rm=T),TPmedian=median(TP,na.rm=T)
                     ,TPmin=min(TP,na.rm=T),TPmax=max(TP,na.rm=T)
                     ,ECmean=mean(EC,na.rm=T),ECmedian=median(EC,na.rm=T)
                     ,ECmin=min(EC,na.rm=T),ECmax=max(EC,na.rm=T))
# Merge n to results dataframe
PA2013results <- merge(PA2013calcs, PAdatawide, by='StationID')

write.csv(PA2013results, file='PA2013results.csv')

# Test Bacteria Function, needs work on data management side
ecoliHit <- function(x){ifelse(x$value>235,1,0)}
ecolidf <- filter(PAdatalong, variable=='EC')


ecolidf2 <- mutate(ecolidf, exceedCount=ecoliHit(ecolidf))
