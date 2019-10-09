library(classInt)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(rworldmap)
library(tidyverse)

# Read in the data

region.final <- read.csv('C:/Users/cjcar/Documents/Github/humanhelminths/Regionized.csv')

# Pull out the iso codes column - this is what gets mapped

demo <- region.final$ISOall

# This restructures them 

isodf <- function(x) {
  x <- strsplit(paste(x, collapse=', '), ', ')[[1]]
  df <- data.frame(table(x))
  names(df) <- c('ISO','Count')
  return(df)
}

# Attach that all to the map
sPDF <- joinCountryData2Map( isodf(demo)
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "ISO")

# Makes the breaks for the legend
classInt <- classIntervals( sPDF[["Count"]]
                            ,n=7, style = "equal")
catMethod = classInt[["brks"]]

# Color palette
colourPalette <- brewer.pal(7,'RdPu')

#mapDevice() #create world map shaped window
mapCountryData(sPDF,
               nameColumnToPlot='Count',
               colourPalette=colourPalette,
               catMethod = catMethod,
               mapTitle = "Number of studies") # Change this to change the title

######################################

# If you want to run one for just a particular species, run it below by changing this line

region.final %>% filter(Latin.name == 'Onchocerca volvulus') -> region.sub

demo <- region.sub$ISOall

isodf <- function(x) {
  x <- strsplit(paste(x, collapse=', '), ', ')[[1]]
  df <- data.frame(table(x))
  names(df) <- c('ISO','Count')
  return(df)
}

sPDF <- joinCountryData2Map( isodf(demo)
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "ISO")

classInt <- classIntervals( sPDF[["Count"]]
                            ,n=7, style = "equal")
catMethod = classInt[["brks"]]

colourPalette <- brewer.pal(7,'RdPu')

#mapDevice() #create world map shaped window
mapCountryData(sPDF,
               nameColumnToPlot='Count',
               colourPalette=colourPalette,
               catMethod = catMethod,
               mapTitle = 'Studies of Onchocerca volvulus')
