## NFL Attendance

library(tidyverse)
library(ggimage)
library(ggrepel)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')


s1 <- standings %>% 
    #filter(team=="Dallas") %>% 
    group_by(year, team_name, sb_winner) %>% 
    summarise(srs=mean(simple_rating)) %>% 
    as_tibble() %>% 
    filter(sb_winner=='Won Superbowl')

s2 <- standings %>% 
    #filter(team=="Dallas") %>% 
    group_by(year, team_name, sb_winner) %>% 
    summarise(srs=mean(simple_rating)) %>% 
    as_tibble() %>% 
    group_by(year) %>% 
    filter(srs==max(srs),sb_winner!='Won Superbowl' ) 
    
sb <- s2 %>% 
    full_join(s1, by="year") %>% 
    #replace_na(list(srs.x=0)) %>% 
    mutate(top_score=if_else(srs.y>=srs.x,TRUE ,FALSE)) %>% 
    replace_na(list(top_score=TRUE)) 

image <- 'lom.jpg'

sb %>% 
    ggplot(aes(x=year))+
    #geom_point(aes(y=srs.y), size=3)+
    geom_image(aes(y=srs.y), image=image)+
    geom_label(aes(y=srs.y-1, label=team_name.y, fill=top_score), size=2.5)+
    #geom_text(aes(y=srs.y-1, label=team_name.y, fill=top_score), angle=90)+
    geom_point(aes(y=srs.x), size=2, colour='red')+
    #geom_label(aes(y=srs.x+1, label=team_name.x))+
    ggtitle("Did the best team win the Superbowl?", 
            subtitle="Best team by Simple rating system (MoV+SoS)")+
    ylab("SRS Rating")+
    xlab("Year")+
    theme_classic()+
    scale_fill_manual(values=c("#999999", "#E69F00"), 
                        name="Winner as\nhighest ranked",
                        breaks=c(TRUE,FALSE),
                        labels=c("Highest Ranked", "Lower Ranked"))
