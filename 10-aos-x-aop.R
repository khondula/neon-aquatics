# dates of sampling closest to flights

library(tidyverse)
library(fs)
library(glue)
library(lubridate)
# library(vroom)
# library(hms)
# library(data.table)

source('R/myfxns.R')

mysite <- aq_site_ids[1]

get_aos_x_aop <- function(mysite){
  aop_dates <- get_aop_dates(mysite) %>% pull(flightdate)
  get_dates_site <- function(my_siteid){
    chem_dir <- '~/Box/data/NEON/NEON_chem-surfacewater'
    lab_files <- fs::dir_ls(glue('{chem_dir}/{mysite}'), glob = "*externalLab*")
    chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
    aos_dates <- chem_df %>% 
      # filter to analyte of interest here
      dplyr::filter(!is.na(analyteConcentration)) %>%
      pull(startDate) %>% 
      lubridate::as_date() %>% unique()
    return(aos_dates)
  }
  
  my_aos_dates <- get_dates_site(mysite)
  
  get_bordering_dates <- function(aop_date){
    
    before_date <- NA
    if(any(which(my_aos_dates < aop_date))){
      before_date_id <- max(which(my_aos_dates < aop_date))
      before_date <- my_aos_dates[before_date_id]
    }
    after_date <- NA
    if(any(which(my_aos_dates > aop_date))){
      after_date_id <- min(which(my_aos_dates > aop_date))
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

aos_x_aop_df <- aq_site_ids %>% purrr::map_dfr(~get_aos_x_aop(.x))

