library(tidyverse)
library(lubridate)
library(gganimate)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo_sightings2 <- ufo_sightings %>% 
    mutate(date=as_date(date_time, format='%m/%d/%Y %H:%M', tz='utc')) %>% 
    mutate(YR=year(date))

library(maps)

world_map <- map_data("world")

p <- ggplot() + 
    geom_polygon(data=world_map,aes(x=long, y=lat,group=group), col="gray50") +
    theme_dark()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill="gray10"))+
    geom_point(data = ufo_sightings2, aes(longitude,latitude, group=YR), colour="green", size=0.1)+
    labs(title = 'UFO Sightings by Year: {frame_time}') +
    transition_time(YR) +
    ease_aes('linear')+
    shadow_mark(alpha=alpha/2)
p

p2 <- animate(p, end_pause = 10, duration=15, nframes=109)
p2

anim_save('UFO.gif')
