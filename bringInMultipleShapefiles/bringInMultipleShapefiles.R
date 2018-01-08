# This script helps read in multiple shapefiles efficiently into R and combine into a single object for further manipulation.
# Created by Emma Jones (emma.jones@deq.virginia.gov)
# Build in R v 3.4.1


library(tidyverse) # version 1.1.1
library(rgdal) # v 1.2-8
library(maptools) # v 0.9-2

# The normal way one brings in a shapefile
# Note, on a Windows machine you need to use the full file location, on Mac/Linux you can use a relative path instead (relative to the project working directory)
wshdSites <- readOGR('C:/Jack/1AXLR000.44/Layers','1AXLR000.44')

# Bring in multiple shapefiles into same list object
i=1
n1 <- '1AXLK000.04'
n2 <- '1AXLR000.44'
test <- readOGR(paste('C:/Jack/',n1,'/Layers',sep=''),n1)
test[[1]] <- readOGR(paste('C:/Jack/',n1,'/Layers',sep=''),n1)
test[[2]] <-  readOGR(paste('C:/Jack/',n2,'/Layers',sep=''),n2)



# Build it into a loop
dirFiles <- list.files(path = "C:/Jack")
test <- list()

for(i in 1:length(dirFiles)){
  test[[i]] <- readOGR(paste('C:/Jack/',dirFiles[i],'/Layers',sep=''),dirFiles[i])
}

# How to look at individual shapefile inside the list
test[[35]]@data

# Tricky way to combine multiple shapefiles into a single file, assuming they have the same projections and CRS
test2 <- spRbind(test[[33]],test[[34]])
# Alternate way if you run into problems with objectID overlap
library(devtools)
install_git("git://github.com/gsk3/taRifx.geo.git")
library(taRifx.geo)
rbind(test[[33]],test[[34]], fix.duplicated.IDs=TRUE)
