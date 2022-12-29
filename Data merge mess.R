library(tidyverse)
library(readxl)

setwd("C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data")

new <- readxl::read_xlsx('lit review FINAL.xlsx')
old <- readxl::read_xlsx('lit review master COMBINED.xlsx')

nrow(new)
nrow(old)

#################################################
# Clean the old data

which(old$Link %in% old$Link[duplicated(old$Link)],) # Investigate some stuff

old[  which(old$Link %in% old$Link[duplicated(old$Link)],) , ]

old$Link[51] <- 'https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0006471'

View(old[c(175, 233),])

old <- old[-175,]

old$`Review?`[233] <- 'Y'

#################################################
# Clean the new data

which(new$Link == 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3564894/')

which(new$Link %in% new$Link[duplicated(new$Link)],) # Investigate some stuff

new[  which(new$Link %in% new$Link[duplicated(new$Link)],) , ] %>% View()

new <- new[-50,]

#################################################
# NEED TO READ IN THE CSV FOR THE CLEAN NAMES

old2 <- read_csv('lit review master COMBINED.csv')

which(old2$Link %in% old2$Link[duplicated(old2$Link)],) # Investigate some stuff

old2[  which(old2$Link %in% old2$Link[duplicated(old2$Link)],) , ] %>% View()

old2 <- old2[-c(405, 455), ]


################################################
# Filter the old data to the new data

old2 %>% filter(Link %in% new$Link) -> old2

# Which columns can be merged?

colnames(new[!(colnames(new) %in% colnames(old2))])
colnames(old2[!(colnames(old2) %in% colnames(new))])

# Merge metholodgy and countries into new stuff

new %>% mutate(Link = gsub(' ','',Link)) -> new
old2 %>% mutate(Link = gsub(' ','',Link)) -> old2

new %>% 
  left_join((old2 %>% select(Link, `Countries pre-coding`)), by = 'Link') -> 
  clean

clean %>%
  select(Citation, Link, `Review?`, `Latin name`, 
         `how many pathogens`, `Specifies on a species level?`,
         `Coendemicity?`, `Coinfection?`,
         Year, `Spatial scale`, `Countries pre-coding`,
         `Methodology_clean`, `Analysis`,
         PAR, Uncertainty, 
         Diagnostics, `Sample size`, 
         Host, Vector, Environment, `Data archived?`,
         `Specific notes`) %>%
  rename(Binomial = `Latin name`,
         Multipathogen = `how many pathogens`,
         SpeciesLevel = `Specifies on a species level?`,
         Coendemicity = `Coendemicity?`,
         Coinfection = `Coinfection?`,
         SpatialScale = 'Spatial scale',
         Countries = `Countries pre-coding`,
         Methodology = `Methodology_clean`,
         Secondary = `Review?`,
         PopAtRisk = `PAR`,
         SampleSize = `Sample size`,
         DataArchival = `Data archived?`,
         Notes = `Specific notes`) -> clean

con<-file('CleanedDataAugust.csv', encoding="UTF-8")

write.csv(clean, con)