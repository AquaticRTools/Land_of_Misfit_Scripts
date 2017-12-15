## Rolling a custom function across a time series of data ##
#  Script credit goes to Matt Dancho
#  Full article: http://www.business-science.io/timeseries-analysis/2017/07/23/tidy-timeseries-analysis-pt-2.html
## Adapted to WQ data and added to AquaticRTools by Emma Jones, emma.jones@deq.virginia.gov

suppressPackageStartupMessages(library(dataRetrieval)) # Grabs NWIS data straight into R
suppressPackageStartupMessages(library(tidyquant))  # Loads tidyverse, tidquant, financial pkgs, xts/zoo


# Most of these functions were built to be nested in larger functions that deal with the rest of the 
# parameters pulled from NWIS. Here we only look at temperature to demonstrate the tq_mutate functionality
# with a custom function.

# Custom function to pull specific real time parameters and implement some initial formatting of data
NWISpull <- function(gageNo,start,end){
  allUnitData <- readNWISuv(siteNumbers=gageNo,
                            parameterCd=c("00010", "00095", "00300", "00400", "63680"),
                            startDate=as.Date(start,"%Y-%m-%d"),
                            endDate=as.Date(end,"%Y-%m-%d"),
                            tz='America/New_York')
  allUnitData <- renameNWISColumns(allUnitData)
}


# Custom function to find exceedances of a threshold
numericThreshold <- function(x, threshold){
  suppressWarnings(
    if(min(x,na.rm=T)==Inf){
      NA
    }else{
      ifelse(min(x,na.rm=T)>threshold,1,0) }
    )
}




## Temperature analysis


# Grab paired upstream/downstream real time gage data for the 2hrs
last2Hours <- format(Sys.time()-7200,format="%Y-%m-%d %H:00:00") # small dataset for 
currentTime <- format(Sys.time(),format="%Y-%m-%d %H:00:00") # small dataset for 

upstreamData <- NWISpull('03171597', last2Hours, currentTime) %>% # pull last 2hrs of data
  dplyr::select(agency_cd,site_no,dateTime,Wtemp_Inst)%>%rename(upstream=!!names(.[4])) # change parameter to general name to make further manipulations easier
downstreamData <- NWISpull('0317159760', last2Hours, currentTime) %>% # pull last 2hrs of data
  dplyr::select(agency_cd,site_no,dateTime,Wtemp_Inst)%>%rename(downstream=!!names(.[4])) # change parameter to general name to make further manipulations easier


# Class VI WQS
maxT <- 20 
natTchange <- 1
maxHourlyTchange <- 0.5 # play around with this threshold to see how tw_mutate function works

# Join the two datasets together and apply WQS
together <- full_join(upstreamData,downstreamData,by=c('agency_cd','dateTime'))%>%
  mutate(numericDiff=downstream-upstream,
         upstreamMaxTviolation=ifelse(upstream>maxT,1,0),# 9VAC25-260-50. Numerical Criteria for Maximum Temperature
         downstreamMaxTviolation=ifelse(downstream>maxT,1,0), # 9VAC25-260-50. Numerical Criteria for Maximum Temperature
         riseAboveNaturalTviolation=ifelse(numericDiff>natTchange,1,0)) %>% # 9VAC25-260-60. Rise Above Natural Temperature
  # 9VAC25-260-70. Maximum Hourly Temperature Change
  tq_mutate(
    select     = numericDiff,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 12,
    align      = "right",
    by.column  = FALSE,
    FUN        = numericThreshold,
    # FUN args
    threshold  = maxHourlyTchange,
    # tq_mutate args
    col_rename = "hourlyTchangeViolation")


# Next you could summarize the outputs however you wish. This tq_mutate is a lifesaver for rolling base or
# custom functions over a window. I haven't seen a cleaner way to do such math yet. 
