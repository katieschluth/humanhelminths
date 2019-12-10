#library(openxlsx)
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

worm <- read_csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data/lit review FINAL.csv')
setwd('C:/Users/cjcar/Documents/Github/humanhelminths/Figures')

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
  `colnames<-`(c('Animal host','count')) %>% drop_na() %>%
  ggplot(aes(x="",y=count, fill=`Animal host`)) + 
    geom_bar(stat='identity', color='grey25', lwd=0.3) + 
    coord_polar('y',start=0, direction=-1) + 
    blank_theme +
    theme(axis.text.x=element_blank()) 
ggsave('Hosts.pdf')

worm %>% separate_rows(Vector, sep=', ') %>% 
  select(Vector) %>% table() %>% data.frame() %>%
  `colnames<-`(c('Animal vector','count')) %>% drop_na() %>%
  ggplot(aes(x="",y=count, fill=`Animal vector`)) + 
  geom_bar(stat='identity', color='grey25', lwd=0.3) + 
  coord_polar('y',start=0, direction=-1) + 
  blank_theme +
  theme(axis.text.x=element_blank()) 
ggsave('Vectors.pdf')


#############################################################

worm %>% separate_rows(`Spatial scale`, sep=', ') %>% 
  select(`Spatial scale`) %>% table() %>% data.frame() %>%
  `colnames<-`(c('Spatial scale','count')) %>% 
  mutate(`Spatial scale`==factor(as.character(`Spatial scale`,levels=c('community',
                                                          'subnational',
                                                          'national',
                                                          'multinational',
                                                          'world')))) %>% 
  ggplot(aes(x="",y=count, fill=`Spatial scale`)) + 
  geom_bar(stat='identity', color='grey25', lwd=0.3) +
  coord_polar('y',start=0, direction=-1) + 
  blank_theme +
  theme(axis.text.x=element_blank())
ggsave('Scales.pdf')

worm %>% separate_rows(`Methodology_clean`, sep=', ') %>% 
  select(`Methodology_clean`) %>% table() %>% data.frame() %>% 
  `colnames<-`(c('Methodology','count')) %>% 
  ggplot(aes(x="",y=count, fill=Methodology)) + 
    geom_bar(stat='identity', color='grey25', lwd=0.3) + 
    coord_polar('y',start=0, direction=-1) + 
    blank_theme +
    theme(axis.text.x=element_blank())
ggsave('Methods.pdf')

###############################################################

worm %>% separate_rows(Diagnostics, sep=', ') %>%
  select(Diagnostics) %>% table() %>% data.frame() %>%
  `colnames<-`(c('Diagnostics','count')) %>% 
  ggplot(aes(x="",y=count, fill=Diagnostics)) + 
    geom_bar(stat='identity', color='grey25', lwd=0.3) +
    coord_polar('y',start=0, direction=-1) + 
    blank_theme +
    theme(axis.text.x=element_blank())
ggsave('Diagnostics.pdf')

