media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

library(tidyverse)

m2 <- media_franchises %>% 
    group_by(original_media, year_created) %>%
    summarise(revenue=sum(revenue)) %>% 
    as_tibble()

m2 %>% 
    filter(!original_media %in% c("Cartoon","Cartoon character","Comic strip", "Digital pet","Greeting card", "Musical theatre","Visual novel")) %>% 
    ggplot(aes(year_created,revenue))+
    geom_area(aes(group=original_media, fill=original_media))+
    facet_grid(original_media~.)+
    theme(strip.background = element_blank(), strip.text.y = element_blank())+#legend.position = 'none')+
    labs(title = "The rise of visual media at the expense of imagination (book reading)",
         subtitle = "Animation/Visual stimuli have overtaken the need to visualise content from books",
         x="")

