# How to make function that outputs ggplot with changeable parameter to plot
library(ggplot2)
library(dplyr)

dataset <- read.csv('data/Briery2.csv')
parameter <- 'DO'

gMasterPlot <- function(dataset,parameter){
  dat <- select(dataset,StationID,date,prettyDate,parameter) %>% # Subset parameter of interest from whole dataset
    na.omit() %>% # Remove samples without parameter measurements
    dplyr::rename(parameter2=!!names(.[4])) # sneaky rename so ggplot will play nicely in function with changeable parameter variable
  
  # Now make the plot
  p <- ggplot(dat, aes(x=date, y=parameter2, group=StationID)) + 
    geom_point(aes(fill=StationID),colour='black',shape=21, size=6)
  # + more plot stuff
    
  return(p)
}

gMasterPlot(Briery2,'DO')
