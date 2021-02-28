# dates of sampling closest to flights

library(tidyverse)
library(fs)
library(glue)
library(lubridate)
# library(vroom)
# library(hms)
# library(data.table)

source('R/myfxns.R')

mysite <- aq_site_ids[9]

# assume that there will be lab data available 
# for all samples
location_types <- NULL

get_aos_x_aop <- function(mysite, location_types = NULL){
  aop_dates <- get_aop_dates(mysite) %>% pull(flightdate)
  
  my_aos_dates <- get_aos_dates(mysite, location_types) %>% 
    pull(collect_date) %>% unique()
  
  get_bordering_dates <- function(aop_date){
    
    before_date <- NA
    if(any(which(my_aos_dates <= aop_date))){
      before_date_id <- max(which(my_aos_dates <= aop_date))
      before_date <- my_aos_dates[before_date_id]
    }
    after_date <- NA
    if(any(which(my_aos_dates >= aop_date))){
      after_date_id <- min(which(my_aos_dates >= aop_date))
      after_date <- my_aos_dates[after_date_id]
    }
    
    return(data.frame(aos_before = before_date,
                      aos_after = after_date))
  }
  
  
  dates_df <- aop_dates %>% 
    purrr::map_dfr(~get_bordering_dates(.x)) %>% 
    mutate(siteid = mysite) %>%
    mutate(flightdate = aop_dates) %>%
    dplyr::select(siteid, flightdate, aos_before, aos_after)
  
  dates_df <- dates_df %>% 
    mutate(days_before = difftime(flightdate, aos_before, units = 'days'),
           days_after = difftime(aos_after, flightdate, units = 'days')) %>%
    mutate(days_before = as.numeric(days_before),
           days_after = as.numeric(days_after))
  
  return(dates_df)
}

get_aos_x_aop('HOPB')

aos_x_aop_df <-  aq_site_ids %>% purrr::map_dfr(~get_aos_x_aop(.x))

aos_x_aop_df <- aos_x_aop_df %>% 
  filter(!is.na(flightdate)) %>%
  rowwise() %>%
  mutate(min_days = min(days_before, days_after, na.rm = TRUE))

aos_x_aop_df %>% write_csv('results/aos-x-aop.csv')

aos_x_aop_df %>%
  ggplot(aes(x = min_days)) +
  geom_histogram(binwidth = 1, fill = 'dodgerblue', col = 'gray', lwd = 0.2) +
  theme_minimal() +
  coord_cartesian(xlim = c(-1, 30)) +
  ggtitle('Days between flight and sampling (< 30)')

ggsave('figs/days-gap.png')
   