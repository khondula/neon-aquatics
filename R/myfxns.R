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

get_aos_dates <- function(mysite, location_types = NULL){
  site_aos_dates <- read_csv('results/all-aos-dates.csv') %>%
    dplyr::filter(siteID == mysite) %>%
    mutate(location_type = substr(namedLocation, 10, nchar(namedLocation)))
  if(!is.null(location_types)){
    site_aos_dates <- site_aos_dates %>%
      dplyr::filter(location_type %in% location_types)
  }
  return(site_aos_dates)
}
get_aos_dates('HOPB', 'S2')
get_aos_dates('CRAM', 'inlet')
