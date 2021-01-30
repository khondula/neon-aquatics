# use neon api to get sensor data
# water quality
library(tidyverse)
# library(httr)
# library(jsonlite)
library(glue)
library(lubridate)

# test out with COMO data
wq_dir <- '~/Box/data/NEON/NEON_water-quality'
siteid <- 'COMO'

# sensor position metadata
# get_swchem_sites <- function(siteid){
  coords <- fs::dir_ls(glue('{wq_dir}/{siteid}'), glob = "*sensor_positions*") %>% 
    purrr::map_df(~read_csv(.x)) %>% 
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, geodeticDatum) %>%
    distinct()
  # return(coords)
# }
# library(neonUtilities)
# 
# base_url <- 'http://data.neonscience.org/api/v0/'
# data_id <- 'DP1.20288.001' # water quality
# 
# req_data_product <- GET(glue('{base_url}/products/{data_id}'))
# avail_data <- content(req_data_product, as = 'text') %>% 
#   fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
# 
# # List of products by site code with month
# # eg ABBY/2017-06
# data_urls_list <- avail_data$data$siteCodes$availableDataUrls
# data_urls <- data_urls_list %>% unlist()
# 
# head(data_urls)
# 
# my_url <- data_urls[1]
# # get all of the water quality data for one aquatic site
# # # actual files available
# data_files_req <- GET(my_url)
# data_files <- content(data_files_req, as = "text") %>% fromJSON()
# data_files$data$files$name
# 
# imgs <- data_files$data$files$name


