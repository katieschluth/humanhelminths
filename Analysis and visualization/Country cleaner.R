library(classInt)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(rworldmap)
library(tidyverse)

#############################################################################################

#### THIS PART OF THE SCRIPT READS IN THE SOURCE INFO ON WHAT REGIONS HAVE
# WHAT MEBER STATES, AND GENERATES A DICTIONARY CALLED 'REGIONS'

# The GBD regions which are within-continent
gbdregions <- readOGR('C:/Users/cjcar/Dropbox/ZikaFuturesCC/GBDregions.shp')

subregions <- gbdregions@data[,c('ISO','NAME_0','Region')]

# MANUALLY recoding a few subregion elements

subregions %>% as_tibble() %>% mutate(Region=recode(Region, 'Asia (South)' = 'South Asia',
                                                    'Europe (Western)' = 'Western Europe',
                                                    'Europe (Central)' = 'Central Europe',
                                                    'Sub-Saharan Africa (Central)' = 'Central Africa',
                                                    'Sub-Saharan Africa (East)' = 'East Africa',
                                                    'Sub-Saharan Africa (West)' = 'West Africa',
                                                    'Asia (High Income Pacific)' = 'East Asia',
                                                    'Asia (East)' = 'East Asia',
                                                    'Northern Europe (Western)' = 'Western Europe',
                                                    'North America (High Income)' = 'North America',
                                                    'Asia (Southeast)' = 'Southeast Asia',
                                                    'Latin America (Central)' = 'Latin America',
                                                    'Latin America (Tropical)' = 'Latin America',
                                                    'Latin America (Andean)' = 'Latin America',
                                                    'Latin America (Southern)' = 'Latin America',
                                                    'Asia (Central)' = 'Central Asia',
                                                    'Europe (Eastern)' = 'Eastern Europe',
                                                    'Sub-Saharan Africa (Southern)' = 'Southern Africa')) -> subregions

unique(subregions$Region)

# Here's more continent stuff
continents <- read.csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/Non-lit review data/helminth continents.csv')

# UNREGION1 and UNREGION2 are good 
# Try subregions > try 2 

# This part of the script puts them together, and fills in a few missing country names (all islands I think)

continents %>% select(ISO, UNREGION1, UNREGION2) %>% as_tibble() -> cont
subregions %>% as_tibble() %>% dplyr::rename(NAME_ENGLI = NAME_0) %>% 
  right_join(cont) -> regions

regions$NAME_ENGLI <- as.character(regions$NAME_ENGLI)

for (i in 1:nrow(regions)) {
  if(is.na(regions[i,'NAME_ENGLI'][[1]])) {
    regions[i,'NAME_ENGLI'] <- as.character(continents[continents$ISO==regions[i,'ISO'][[1]],'NAME_ENGLI'][[1]])
  }
}

View(regions[!complete.cases(regions),])

# OK, regions is now a working dictionary.

##########################################################################################

# This part is an extension where we make a couple things that can be dropped in
# to replace the regions

commas <- function(x) {if(length(x)==1) {return(x)} else {paste(unique(x), collapse=', ')}}
commas(c('Cameroon','Chad','Chad','Algeria'))

smallregions <- data.frame(aggregate(NAME_ENGLI ~ Region, regions, commas))
bigregions <- data.frame(aggregate(NAME_ENGLI ~ UNREGION2, regions, commas))


##########################################################################################

# The helminth data
raw <- read.csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data/lit review master COMBINED.csv')

raw %>% mutate(countries.pre.coding = str_replace_all(as.character(Countries.pre.coding), 
                                                  pattern=' and ',
                                                  replacement=', ')) %>% 
  separate_rows(countries.pre.coding, sep=',') %>%
  left_join(regions %>% select(ISO, NAME_ENGLI) %>% dplyr::rename(countries.pre.coding = NAME_ENGLI) %>% unique()) -> 
  raw.split

bigboys <- structure(as.character(bigregions$NAME_ENGLI), names = as.character( bigregions$UNREGION2))
ittybittyboys <- structure(as.character(smallregions$NAME_ENGLI), names = as.character( smallregions$Region))

raw.split %>% 
  mutate(Countries.Coded = trimws(as.character(countries.pre.coding))) %>% 
  mutate(Countries.Coded = recode(Countries.Coded, !!!ittybittyboys)) %>%
  mutate(Countries.Coded = recode(Countries.Coded, !!!bigboys)) %>%
  separate_rows(Countries.Coded, sep=',') %>% 
  mutate(Countries.Coded = recode(Countries.Coded, !!!ittybittyboys)) %>%
  mutate(Countries.Coded = recode(Countries.Coded, !!!bigboys)) %>%
  select(-c(Spatial.scope, Countries.cleaned, Countries.pre.coding.flag, 
          Countries.pre.coding, countries.pre.coding)) %>% distinct() -> region.clean 

regions %>% select(ISO, NAME_ENGLI) %>% dplyr::rename(Countries.Coded = NAME_ENGLI) %>% unique() -> isos

for (i in 1:nrow(region.clean)) {
  if(is.na(region.clean$ISO[i])){
    country = trimws(region.clean$Countries.Coded[i])
    isoi <- isos[isos$Countries.Coded==country,'ISO'][[1]]
    if(length(isoi)>0) {region.clean$ISO[i] <- isoi}
  }
}

###################### MANUAL CLEANING 

unique(region.clean[is.na(region.clean$ISO),'Countries.Coded'])

region.clean %>% mutate(Countries.Coded = recode(Countries.Coded, 
                                                 ' Bonaire' = 'Bonaire, Sint Eustatius and Saba',
                                                   " Sint Eustatius and Saba" = 'Bonaire, Sint Eustatius and Saba',
                                                   " Virgin Islands"='Virgin Islands, U.S.',
                                                   ' U.S.'='Virgin Islands, U.S.',
                                                   'Congo'='Republic of Congo',
                                                   'The Gambia'='Gambia',
                                                   'Guinea Bissau'='Guinea-Bissau',
                                                   'São Tomé'='Sao Tome and Principe',
                                                   'Príncipe'='Sao Tome and Principe',
                                                   'CAR'='Central African Republic',
                                                   'Democratic Republic of Congo'='Democratic Republic of the Congo',
                                                   'DRC'='Democratic Republic of the Congo',
                                                   'Uganda.' = 'Uganda',
                                                   'Ivory Coast'='Cote D\'Ivoire',
                                                   "Côte d'Ivoire"='Cote D\'Ivoire',
                                                   'Palestine'='Palestina',
                                                   'England'='United Kingdom',
                                                   'Scotland'='United Kingdom',
                                                   'Wales'='United Kingdom',
                                                   'Southern Sudan'='South Sudan',
                                                   'Nigria'='Nigeria',
                                                   'Northern Ireland'='United Kingdom',
                                                   'America Samoa'='American Samoa')) -> region.clean

for (i in 1:nrow(region.clean)) {
  if(is.na(region.clean$ISO[i])){
    country = trimws(region.clean$Countries.Coded[i])
    isoi <- isos[isos$Countries.Coded==country,'ISO'][[1]]
    if(length(isoi)>0) {region.clean$ISO[i] <- isoi}
  }
}

unique(region.clean[is.na(region.clean$ISO),'Countries.Coded'])



subregionsalt <- gbdregions@data[,c('ISO','NAME_0','Region')]
subregionsalt %>% as_tibble() %>% mutate(Region=recode(Region, 'Asia (South)' = 'South Asia',
                                                    'Europe (Western)' = 'Western Europe',
                                                    'Europe (Central)' = 'Central Europe',
                                                    'Sub-Saharan Africa (Central)' = 'Sub-Saharan Africa',
                                                    'Sub-Saharan Africa (East)' = 'Sub-Saharan Africa',
                                                    'Sub-Saharan Africa (West)' = 'Sub-Saharan Africa',
                                                    'Asia (High Income Pacific)' = 'East Asia',
                                                    'Asia (East)' = 'East Asia',
                                                    'Northern Europe (Western)' = 'Western Europe',
                                                    'North America (High Income)' = 'North America',
                                                    'Asia (Southeast)' = 'Southeast Asia',
                                                    'Latin America (Central)' = 'Central America',
                                                    'Latin America (Tropical)' = 'South America',
                                                    'Latin America (Andean)' = 'South America',
                                                    'Latin America (Southern)' = 'South America',
                                                    'Asia (Central)' = 'Central Asia',
                                                    'Europe (Eastern)' = 'Eastern Europe',
                                                    'Sub-Saharan Africa (Southern)' = 'Sub-Saharan Africa')) -> subregionsalt
moreboys <- structure(as.character(subregionsalt$NAME_0), names = as.character(subregionsalt$Region))


region.clean %>% mutate(Countries.Coded = recode(Countries.Coded, !!!moreboys)) %>% 
  mutate(Countries.Coded = recode(Countries.Coded, 
                                                 'North Africa' = 'Algeria, Egypt, Libya, Morocco, Tunisia, Western Sahara',
                                                 'Northern Africa' = 'Algeria, Egypt, Libya, Morocco, Tunisia, Western Sahara',
                                                 'Middle East' = 'Bahrain, Iran, Iraq, Jordan, Kuwait, Lebanon, Oman, Palestina, Qatar, Saudi Arabia, Syria, Turkey, United Arab Emirates, Yemen',
                                                 'the Middle East' = 'Bahrain, Iran, Iraq, Jordan, Kuwait, Lebanon, Oman, Palestina, Qatar, Saudi Arabia, Syria, Turkey, United Arab Emirates, Yemen',
                                                 'Former USSR' = 'Armenia, Moldova, Estonia, Latvia, Lithuania, Georgia, Azerbaijan, Tajikistan, Kyrgyzstan, Belarus, Uzbekistan, Turkmenistan, Ukraine, Kazakhstan, Russia')) %>%
  separate_rows(Countries.Coded, sep=',') -> region.clean

for (i in 1:nrow(region.clean)) {
  if(is.na(region.clean$ISO[i])){
    country = trimws(region.clean$Countries.Coded[i])
    isoi <- isos[isos$Countries.Coded==country,'ISO'][[1]]
    if(length(isoi)>0) {region.clean$ISO[i] <- isoi}
  }
}

unique(region.clean[is.na(region.clean$ISO),'Countries.Coded'])

######################

region.clean %>% as_tibble() %>%
  group_by(UNIQUE.ID) %>% 
  mutate(ISOall = paste0(ISO, collapse=', ')) %>%
  select(-c(ISO, Countries.Coded)) %>% 
  distinct() -> region.final



###################################################################################

# This is a function that takes a given column you pull from the data that looks like
# ISO
# ISO, ISO, ISO, ISO...
# ISO, ISO
# and adheres it to GADM, and then makes a map!

# par(mfrow=c(1,1))

demo <- region.final$ISOall

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
mapCountryData(sPDF
               ,nameColumnToPlot='Count',
               ,colourPalette=colourPalette
               ,catMethod = catMethod)



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
mapCountryData(sPDF
               ,nameColumnToPlot='Count',
               ,colourPalette=colourPalette
               ,catMethod = catMethod)
