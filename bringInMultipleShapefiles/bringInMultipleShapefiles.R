# This script helps combine Jack's point and polygon data into a single file bc ArcGIS can't merge them without errors
library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)

#library(maptools)
#library(reshape)
#library(reshape2)
#library(plyr)
#library(dplyr)

# Normal Way 
wshdSites <- readOGR('C:/Jack/1AXLR000.44/Layers','1AXLR000.44')

# Test bringing into same list object
i=1
n1 <- '1AXLK000.04'
n2 <- '1AXLR000.44'
test <- readOGR(paste('C:/Jack/',n1,'/Layers',sep=''),n1)
test[[1]] <- readOGR(paste('C:/Jack/',n1,'/Layers',sep=''),n1)
test[[2]] <-  readOGR(paste('C:/Jack/',n2,'/Layers',sep=''),n2)



# Loop it
dirFiles <- list.files(path = "C:/Jack")
dirFilesShort <- dirFiles[33:35]
test <- list()

for(i in 33:35){#length(dirFiles)-1){
  test[[i]] <- readOGR(paste('C:/Jack/',dirFiles[i],'/Layers',sep=''),dirFiles[i])
}

test[[35]]@data


test2 <- spRbind(test[[33]],test[[34]])

library(devtools)
install_git("git://github.com/gsk3/taRifx.geo.git")
library(taRifx.geo)
rbind(test[[33]],test[[34]], fix.duplicated.IDs=TRUE)
