# convenience functions
library(magrittr)

aq_site_ids <- readr::read_lines('aq_site_ids.txt')

get_aop_dates <- function(aq_siteids){
  aop_file <- 'results/sites_join_aop_dates.csv'
  aop_dates <- readr::read_csv(aop_file, col_types = 'ccccccccddD') %>%
    dplyr::filter(siteID %in% aq_siteids) %>%
    dplyr::select(siteID, aop_site_id, flightdate) %>%
    dplyr::arrange(flightdate) %>% distinct()
  return(aop_dates)
}