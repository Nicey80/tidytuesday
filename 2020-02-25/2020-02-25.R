library(tidyverse)

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

head(measles)

##Test set to reviw the data
# m_AZ <- measles %>% 
#     mutate(vac=enroll*mmr) %>% 
#     filter(state=="Arizona") %>% 
#     group_by(county) %>% 
#     summarise(enr=sum(enroll, na.rm = T), vac=sum(vac, na.rm = T)) %>%
#     as_tibble() %>% 
#     mutate(vac_pct=vac/enr)


#Extract and summarise Cali mmr data by county for plotting
meas <- measles %>% 
    mutate(join_key=paste(state,county,"County", sep = " ")) %>% 
    mutate(vac=enroll*mmr/100) %>% 
    filter(state=="California", mmr>0) %>% #exclude those without records mmr=-1
    group_by(join_key) %>% 
    summarise(enr=sum(enroll, na.rm = T), vac=sum(vac, na.rm = T)) %>%
    as_tibble() %>% 
    mutate(vac_pct=vac/enr*100) # vaccination rate

#extract the low vaccination hotspots
meas_cal <- measles %>% 
    mutate(join_key=paste(state,county,"County", sep = " ")) %>% 
    mutate(vac=enroll*mmr/100) %>% 
    filter(state=="California", mmr>0, mmr<50) #%>% 
    #mutate(vac_pct=vac/enr)

library(tigris)
library(ggplot2)
library(ggthemes)

#extract the counties data from the tigris package
me <- counties(state = "California")


# convert to sf
me2 <- sf::st_as_sf(me)


#extract fips codes to join stae & county
data("fips_codes")

fips_codes <- fips_codes %>% 
    filter(state_name=="California") %>% 
    mutate(join_key=paste(state_name,county, sep=" "))

fips_state <- fips_codes %>% 
    select(state_code, state_name) %>% 
    distinct()
    #summarise_all() %>% 
    as_tibble()


me2 <- me2 %>% 
    left_join(fips_state, by=c("STATEFP"="state_code")) %>% 
    mutate(join_key=paste(state_name,NAMELSAD, sep=" "))

#join fips codes to AZ dataset
#to allow join to full data

me2 <- me2 %>% 
    left_join(meas)#, by=c("NAME"="county"))

#plot of state
p1 <- me2 %>%
    filter(vac_pct>0, !is.na(vac_pct)) %>% 
    ggplot()+
    geom_sf(aes(fill=vac_pct))+
    geom_point(data=meas_cal, aes(lng,lat), colour='red')+
    scale_fill_continuous() +
    #ggtitle("MMR Vacination Rates in California Counties", subtitle = "Plus facilites <50% coverage") +
    theme_map()+
    labs(var_pct="Vacination \npercent")+
    theme(legend.title = element_text(size=8), legend.text = element_text(size=8))

p1


#focus on county with lowest vaccination rate in smaller plot
meas_mendo <-  measles %>% 
    mutate(join_key=paste(state,county,"County", sep = " ")) %>% 
    mutate(vac=enroll*mmr/100) %>% 
    filter(state=="California", mmr>0, county=="Mendocino") %>% 
    filter(lng>-124, lng<-122, lat>38.2, lat<40.5) %>% 
    mutate(non_vac_rate=100-mmr)

me_mend <- sf::st_as_sf(me) %>% 
    filter(NAME=="Mendocino")

me_mend %>% ggplot() +
    geom_sf(fill="grey") +
    geom_point(data=meas_mendo, aes(lng,lat, size=enroll-vac))+
    theme_map()+
    theme(legend.title = element_text("xx"))


library(ggmap)

ph_basemap <- get_map(location=c(lon =-123.5, lat = 39.4), 
                      zoom=9, maptype = 'roadmap', source = 'google', color = "bw")
ggmap(ph_basemap)


p2 <- ggmap(ph_basemap)+
    geom_sf(data=me_mend, fill="darkblue", alpha=0.5, inherit.aes=FALSE)+
    geom_point(data=meas_mendo, aes(lng,lat, size=non_vac_rate))+
    theme(axis.ticks = element_blank(), axis.text = element_blank(),
          axis.title = element_blank(), legend.title = element_text(size=8),
          legend.text = element_text(size=8))+
    #ggtitle("Mendocino County", subtitle = "Non Vacination Hotspots", caption="Mendocino County Hotspots")+
    labs(size="Non Vacination \nRate", caption = "Mendocino County Hotspots")
p2

library(patchwork)




(p1 |(p2/plot_spacer())) + plot_layout(guides = 'collect', widths = c(2,1)) + plot_annotation(
    title = 'MMR Vacination Rates in California Counties',
    subtitle = 'Highlighting facilities with <50% vacination rates',
    caption = 'Pullout: Mendocino County'
)
   
