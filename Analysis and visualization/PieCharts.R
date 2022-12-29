#library(openxlsx)
library(tidyverse)
library(scales)
library(patchwork)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

worm <- read_csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/FinalDataFigs/WormMapsDB-Grid view.csv')
setwd('C:/Users/cjcar/Documents/Github/humanhelminths/FinalDataFigs')

level_key <- c(fox = "wildlife",
               antelope = "wildlife",
               'water buffalo' = "buffalo",
               'wild boar' = "wildlife",
               baboon = "wildlife",
               bobcat = "wildlife",
               human = NA,
               livestock = NA)

worm %>% separate_rows(Host, sep=',') %>% 
  mutate(Host = recode(Host, !!!level_key)) %>%
  select(Host) %>% table() %>% data.frame() %>%
  `colnames<-`(c('Animal host','count')) %>% drop_na() %>%
  ggplot(aes(x="",y=count, fill=`Animal host`)) + 
    geom_bar(stat='identity', color='grey25', lwd=0.3) + 
    coord_polar('y',start=0, direction=-1) + 
    blank_theme +
    theme(axis.text.x=element_blank()) +
    scale_fill_manual(values = ggthemes::tableau_color_pal(palette = "Classic Cyclic")(12)) -> g1 #+ 
    #scale_fill_jama()
#ggsave('Hosts.pdf')

worm %>% separate_rows(Vector, sep=', ') %>% 
  select(Vector) %>% table() %>% data.frame() %>%
  `colnames<-`(c('Animal vector','count')) %>% drop_na() %>%
  ggplot(aes(x="",y=count, fill=`Animal vector`)) + 
  geom_bar(stat='identity', color='grey25', lwd=0.3) + 
  coord_polar('y',start=0, direction=-1) + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  scale_fill_manual(values = awtools::mpalette) -> g2 #+ -

g1 + g2
#ggsave('Vectors.pdf')


#############################################################

worm %>% separate_rows(`SpatialScale`, sep=', ') %>% 
  select(`SpatialScale`) %>% table() %>% data.frame() %>%
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
  theme(axis.text.x=element_blank()) +
  scale_fill_manual(values = awtools::a_palette[6:2]) -> g3
#ggsave('Scales.pdf')

worm %>% separate_rows(`Methodology`, sep=',') %>% 
  select(`Methodology`) %>% table() %>% data.frame() %>% 
  `colnames<-`(c('Methodology','count')) %>% 
  ggplot(aes(x="",y=count, fill=Methodology)) + 
    geom_bar(stat='identity', color='grey25', lwd=0.3) + 
    coord_polar('y',start=0, direction=-1) + 
    blank_theme +
    theme(axis.text.x=element_blank()) + 
    scale_fill_manual(values = ggthemes::tableau_color_pal(palette = "Classic Cyclic")(13)[c(1,3,5,7,9,11,13)]) -> g4
#ggsave('Methods.pdf')

g4 + g3

###############################################################

worm %>% separate_rows(Diagnostics, sep=',') %>%
  select(Diagnostics) %>% na.omit() %>% table() %>% data.frame() %>%
  `colnames<-`(c('Diagnostics','count')) %>% 
  ggplot(aes(x="",y=count, fill=Diagnostics)) + 
    geom_bar(stat='identity', color='grey25', lwd=0.3) +
    coord_polar('y',start=0, direction=-1) + 
    blank_theme +
    theme(axis.text.x=element_blank()) + 
  scale_fill_manual(values = ggthemes::tableau_color_pal(palette = "Classic Cyclic")(13)[c(1,3,5,7,9,11,13)]) 
ggsave('Diagnostics.pdf')

