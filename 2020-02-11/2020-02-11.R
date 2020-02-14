# Get the Data

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

library(tidyverse)


summary(hotels)

hotels_df <- hotels %>% 
    mutate(arr_date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
           arr_date = parse_date(arr_date, format = "%Y-%B-%d"),
           tot_nights=stays_in_weekend_nights+stays_in_week_nights) %>%
    select(arr_date,everything()) #%>% 
    # tsibble::as_tsibble(index=arr_date)


canc_by_lead <- hotels_df %>% 
    group_by(lead_time,is_canceled, hotel) %>% 
    tally() %>% 
    as_tibble() %>% 
    spread(is_canceled,n) %>% 
    mutate(canc_rate=`1`/(`0`+`1`),
           tot_bookings=`0`+`1`) %>% 
    group_by(hotel) %>% 
    mutate(tot_pct=sum(tot_bookings, na.rm = TRUE),
           cum=cumsum(tot_bookings)) %>% 
    ungroup() %>% 
    mutate(tot_pct2=tot_bookings/tot_pct,
           cum_pct=cum/tot_pct)

p1 <- canc_by_lead %>% 
    ggplot(aes(lead_time,canc_rate, group=hotel, colour=hotel))+ 
    geom_smooth()+
    #geom_col(aes(y=cum_pct,alpha=0.2,))+
    theme_bw()+
    ggtitle("Cancellations by Lead Time")+
    ylab("Cancellation Rate")+
    xlab("Lead Time for Booking")

p1

canc_by_dur <- hotels_df %>% 
    group_by(tot_nights,is_canceled, hotel) %>% 
    tally() %>% 
    as_tibble() %>% 
    spread(is_canceled,n) %>% 
    mutate(canc_rate=`1`/(`0`+`1`))

p2 <- canc_by_dur %>% 
    ggplot(aes(tot_nights,canc_rate, group=hotel, colour=hotel))+ 
    geom_smooth()+
    geom_vline(aes(xintercept=14))+
    theme_bw()+
    ggtitle("Cancellations by Stay Duration")+
    ylab("Cancellation Rate")+
    xlab("Stay Duration")

p2

library(gridExtra)
grid.arrange(p1,p2)

