# Example_Using_assignDistinct_function.r
# Purpose: Provide an example using the assignDistinct() function (final version 
# created 1/25/2017 by Karen Blocksom)
#
# To use, save example files and assignDistinct.r file in working directory.
###################################################################################

source("assignDistinct.r")

# First read in example zooplankton data
zoopEx <- read.csv("zoopEx.csv",stringsAsFactors=F)

# Call assignDistinct() for this data frame
zpdist <- assignDistinct(zoopEx,c('UID'),taxlevels=c('PHYLUM','CLASS','SUBCLASS','ORDER','SUBORDER',
                                                                  'FAMILY','GENUS','SPECIES','SUBSPECIES'))

# Compare previously assigned IS_DISTINCT_300 to the IS_DISTINCT variable just assigned
filter(zpdist,IS_DISTINCT!=IS_DISTINCT_300)


# Now repeat with benthic macroinvertebrate data
benthicEx <- read.csv('benthicEx.csv',stringsAsFactors=F)

# Run function on this data frame
bentdist <- assignDistinct(benthicEx,c('UID','SAMPLE_TYPE'),taxlevels=c('PHYLUM','CLASS','ORDER','FAMILY','SUBFAMILY',
                                                                        'TRIBE','GENUS')
                           ,special.taxa=c('THIENEMANNIMYIA GENUS GR.', 'CERATOPOGONINAE', 'CRICOTOPUS/ORTHOCLADIUS')
                           ,final.name='TARGET_TAXON')
# Compare original value to value just assigned
filter(bentdist,IS_DISTINCT_ORIG!=IS_DISTINCT)
