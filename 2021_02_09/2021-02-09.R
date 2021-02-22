install.packages('tidytuesdayR')
install.packages('ggrepel')

tuesdata <- tidytuesdayR::tt_load('2021-02-09')
tuesdata

tt <- tuesdata$income_distribution
tt

library(tidyverse)
library(scales)
library(ggrepel)

str(tt)
summary(tt)
unique(tt$race)

tt %>% ggplot(aes(x=year, y=income_median)) + geom_line(aes(group=race, colour=race))


tt2 <- tt %>% filter(year==2019) %>% group_by(year,race) %>% 
    summarise(med=mean(income_median)) %>% as_tibble()

tt %>% ggplot(aes(x=year, y=income_median)) + 
    geom_line(data=tt %>% filter(race!="All Races"),aes(group=race, colour=race))+
    geom_line(data=tt %>% filter(race=="All Races"),colour="yellow",size=1, linetype=3)+
    geom_label_repel(data=tt2,aes(x=2030, y=med,label=race), nudge_x = 0)+
    ggtitle(label="Racial Income Inequaltiy in USA", subtitle = "Median income since the financial crash of 2008 has grown much slower in the Black population")+
    theme(legend.position = 'none', 
          axis.title = element_blank(), 
          text = element_text(family = "Tahoma", colour = "green"), 
          axis.text = element_text(colour = "green"), 
          plot.background = element_rect(fill="black"),
          panel.background=element_rect(fill="black"))+
    scale_y_continuous(labels=label_dollar(scale = 0.001, suffix = "k"))
