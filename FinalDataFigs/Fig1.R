
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

helm <- read_csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/FinalDataFigs/WormMapsDB-Grid view.csv')
helm %>% separate_rows(Binomial, sep=', ') %>%
  separate_rows(Binomial, sep=' and ') %>% 
  mutate(Latin = str_replace(Binomial, 'and ', '')) %>%
  mutate(Latin = recode(Latin, !!!recoder)) -> helm
  
df <- data.frame(table(helm$Latin))
head(df)

df %>% filter(!(Var1=='Misc.')) -> df

library(ggthemr)
ggthemr('fresh')
ggplot(df, aes(x=reorder(Var1,-Freq), y=Freq)) + geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0.5)) + xlab('') + ylab('Number of studies')



##########

syn <- read_csv('C:/Users/cjcar/Documents/GitHub/humanhelminths/Lit review data/Syndromes.csv')
syn <- syn[,1:3]
syn <- syn[-27,]
syn <- syn[-9,]

syn$`species from lit review`[15] <- paste(syn$`species from lit review`[15], 'dirofilariosis', sep = ',')
syn$`species from lit review`[3] <- paste(syn$`species from lit review`[3], 'hookworm', sep = ',')
syn$`species from lit review`[2] <- paste(syn$`species from lit review`[2], 'LF', sep = ',')
syn$`species from lit review`[7] <- paste(syn$`species from lit review`[7], 'Onchocerciasis', sep = ',')
syn$`species from lit review`[1] <- paste(syn$`species from lit review`[1], 'schistosomiasis', sep = ',')
syn$`species from lit review`[1] <- paste(syn$`species from lit review`[1], 'schistosomiasis', sep = ',')

syn[26,1] <- 'Soil-transmitted (misc.)'
syn[26,2] <- 'soil-transmitted helminths'

syn$`species from lit review`[12] <- paste(syn$`species from lit review`[12], 'Taenia solium, Taenia spp.', sep = ',')


for (i in 1:nrow(syn)) {
  sp <- trimws(str_split(syn$`species from lit review`[i],',')[[1]])
  
  tally = 0
  
  for (j in 1:nrow(helm)){
    he <- trimws(str_split(helm$Binomial[j],',')[[1]])
    print(he)
    if(sum(he %in% sp)>0) {tally <- tally + 1}
    print(tally)
  }
  
  syn$`number of studies`[i] <- tally
}

library(ggthemr)
ggthemr('fresh')
ggplot(syn, aes(x=reorder(condition,-`number of studies`), y=`number of studies`)) + geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0.5, size = 9)) + xlab('') + ylab('Number of studies') -> g1

############

helm %>% select(Year) %>% ggplot(aes(x = Year, stat = 'identity')) + geom_bar(width = 0.9) + ylab("Number of studies") -> g2

library(patchwork)

(g2 / g1) +
  plot_annotation(tag_levels = 'A')


