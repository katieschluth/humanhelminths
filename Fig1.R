
library(tidyverse)

recoder <- c(hookworm = 'Hookworm (misc.)',
             LF = 'Lymphatic filariasis (misc.)',
             Hookworm = 'Hookworm (misc.)',
             Schistosomiases = 'Schistosoma spp.',
             'Intestinal helminths' = 'Misc.',
             'Soil-transmitted helminthiases' = 'STH (misc.)',
             'soil-transmitted helminthiases' = 'STH (misc.)',
             'soil-transmitted helminths' = 'STH (misc.)',
             'Dirofilariosis' = 'Dirofilaria spp.',
             schistosomiases = 'Schistosoma spp.',
             schistosomiasis = 'Schistosoma spp.',
             Trematodes = 'Misc.',
             trematodes = 'Misc.',
             'Trichuris Trichiura' = 'Trichuris trichiura')

helm <- read.csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data/lit review master COMBINED.csv')
helm %>% separate_rows(Latin.name, sep=', ') %>%
  separate_rows(Latin.name, sep=' and ') %>% 
  mutate(Latin = str_replace(Latin.name, 'and ', '')) %>%
  mutate(Latin = recode(Latin, !!!recoder)) -> helm
  
df <- data.frame(table(helm$Latin))
head(df)

df %>% filter(!(Var1=='Misc.')) -> df

library(ggthemr)
ggthemr('fresh')
ggplot(df, aes(x=reorder(Var1,-Freq), y=Freq)) + geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0.5)) + xlab('') + ylab('Number of studies')



##########

df <- read.csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data/Syndromes.csv')
df <- df[-27,]

library(ggthemr)
ggthemr('fresh')
ggplot(df, aes(x=reorder(condition,-number.of.studies), y=number.of.studies)) + geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0.5, size = 9)) + xlab('') + ylab('Number of studies')

