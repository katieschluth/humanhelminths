library(openxlsx)
library(tidyverse)
library(scales)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

worm <- read.xlsx('C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data/lit review master COMBINED.xlsx')

level_key <- c(fox = "wildlife",
               antelope = "wildlife",
               'water buffalo' = "wildlife",
               'wild boar' = "wildlife",
               baboon = "wildlife",
               bobcat = "wildlife",
               human = NA,
               livestock = NA)

worm %>% separate_rows(Host, sep=', ') %>% 
  mutate(Host = recode(Host, !!!level_key)) %>%
  select(Host) %>% table() %>% data.frame() %>%
  `colnames<-`(c('sample','count')) %>% drop_na() %>%
  ggplot(aes(x="",y=count, fill=sample)) + 
  geom_bar(stat='identity') + 
  coord_polar('y',start=0) + 
  blank_theme +
  theme(axis.text.x=element_blank()) 

worm %>% separate_rows(`Category.(methodology)`, sep=', ') %>% 
  #mutate(Host = recode(Host, !!!level_key)) %>%
  select(`Category.(methodology)`) %>% table() %>% pie()

worm %>% separate_rows(Diagnostics, sep=', ') %>% 
  #mutate(Host = recode(Host, !!!level_key)) %>%
  select(Diagnostics) %>% table() %>% data.frame() %>%
  `colnames<-`(c('sample','count')) %>% 
  ggplot(aes(x="",y=count, fill=sample)) + 
  geom_bar(stat='identity') + coord_polar('y',start=0) + 
  blank_theme +
  theme(axis.text.x=element_blank())
