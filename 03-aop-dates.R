# Get timing of images for AOP data
# adapted from
# https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-usage
# use IMAGEDATETIME in digital camera file names

library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)

## table of flight dates
aop_dates <- read_csv('~/Documents/data/NEON/meta/flight.dates.AOP.csv')
aop_dates <- aop_dates %>% 
  mutate(year = substr(YearSiteVisit, 1, 4),
         siteid = substr(YearSiteVisit, 6, 9),
         flightdate = ymd(substr(FlightDate, 1, 8)))
aop_dates %>% write_csv('results/all_aop_dates.csv')

sites_join_aop_dates <- sites_x_aop %>% left_join(aop_dates, by = c("aop_site_id" = "siteid"))

# aquatic to aop sites

### Or from API ###
base_url <- 'http://data.neonscience.org/api/v0/'
# hs_data_id <- 'DP3.30010.001'
data_id <- 'DP1.30010.001' # digital camera 10cm imagery

req_aop <- GET(glue('{base_url}/products/{data_id}'))
avail_aop <- content(req_aop, as = 'text') %>% 
  # readLines() %>%
  fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

# List of products by site code with month
# eg ABBY/2017-06
data_urls_list <- avail_aop$data$siteCodes$availableDataUrls
data_urls <- data_urls_list %>% unlist()

# make this into a table
avail_df <- data_urls_list %>%
  purrr::map(~str_sub(.x, 56, 67)) %>%
  unlist() %>% as.data.frame() %>%
  mutate(siteid = str_sub(., 1, 4)) %>%
  mutate(month = str_sub(., 6, 12)) %>%
  dplyr::select(siteid, month)

my_url <- data_urls[1]
# actual files available
get_img_datetimes <- function(my_url){
  data_files_req <- GET(my_url)
  data_files <- content(data_files_req, as = "text") %>% fromJSON()
  # filter to just the tifs
  imgs <- data_files$data$files$name %>% fs::path_filter("*ort.tif")
  # extract image dates from parenthesis
  img_datetimes <- imgs %>% 
    str_match_all("(?<=\\().+?(?=\\))") %>% 
    unlist() %>% sort() %>% lubridate::as_datetime()
  # one row data frame of results
  meta <- data.frame(siteid = data_files$data$siteCode,
             month = data_files$data$month,
             first_img = head(img_datetimes, 1),
             last_img = tail(img_datetimes, 1))
  return(meta)
}
get_img_datetimes(data_urls[13])

poss_get_img_datetimes <- purrr::possibly(get_img_datetimes, otherwise = NULL)
aop_meta_df <- data_urls %>% purrr::map_df(~poss_get_img_datetimes(.x))

aop_meta_df %>% write_csv('results/aop_meta_df.csv')
# data_files_req <- GET(data_urls[1])
# data_files <- content(data_files_req, as = "text") %>% fromJSON()
# 
# data_files$data$siteCode
# data_files$data$month
# data_files$data$files$name[1]
#  Digital camera: FLHTSTRT_EHCCCCCC(IMAGEDATETIME)-NNNN_ort.tif
# IMAGEDATETIME: Date and time of image capture, YYYYMMDDHHmmSS

