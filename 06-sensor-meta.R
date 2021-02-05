library(tidyverse)
library(fs)
library(glue)
library(sf)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)

positions_dir <- '~/Box/data/NEON/spatial/wq-sensor-positions'
aq_site_ids <- read_lines('aq_site_ids.txt')

mysite <- 'FLNT'

# get all sensor positions for a site with API

base_url <- 'http://data.neonscience.org/api/v0/'
data_id <- 'DP1.20288.001' # water quality
req_avail <- GET(glue('{base_url}/products/{data_id}'))
avail_resp <- content(req_avail, as = 'text') %>% 
  fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

# List of products by site code with month
data_urls_list <- avail_resp$data$siteCodes$availableDataUrls
# make table of urls with site and months
avail_df <- data_urls_list %>%
  unlist() %>% as.data.frame() %>%
  dplyr::rename(url = 1) %>%
  mutate(siteid = str_sub(url, 56, 59)) %>%
  mutate(month = str_sub(url, 61, 67)) %>%
  dplyr::select(siteid, month, url)

my_site_urls <- avail_df %>% 
  dplyr::filter(siteid == mysite) %>%
  # add line here to filter for filter dates after what is already downloaded?
  pull(url)

my_url <- my_site_urls[1]
# filter to just the waq_instantaneous basic files
get_positions_files <- function(my_url){
  data_files_req <- GET(my_url)
  data_files <- content(data_files_req, as = "text") %>%
    fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
  data_files_df <- data_files$data$files %>% 
    filter(str_detect(name, "sensor_positions"))
  # future enhancement: check md5 sums for changes! 
  # compare to existing files!
  return(list(files = data_files_df$name, urls = data_files_df$url))
}

my_files_list <- my_site_urls %>% purrr::map(~get_positions_files(.x))
fs::dir_create(glue('{positions_dir}/{mysite}'))

download_month <- function(my_files){
  my_files_local <- glue('{positions_dir}/{mysite}/{my_files$files}')
  purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
}
my_files_list %>% purrr::walk(~download_month(.x))
##

coords_df <- glue('{positions_dir}/{mysite}') %>%
  fs::dir_ls(glob = "*sensor_positions*") %>% 
  purrr::map_dfr(~read_csv(.x, col_types = c("dccTlccTlddddddddd"))) %>%
  dplyr::select(HOR.VER, name, description, xOffset, yOffset, zOffset, 
                referenceLatitude, referenceLongitude, referenceElevation) %>%
  distinct()

head(coords_df)

## TODO: function to get sensor positions for all aq sites ##
# assume all lats and long in crs 4326?
# what units are offset in? 

sites_sf <- coords_df %>% sf::st_as_sf(coords = c("referenceLongitude", "referenceLatitude"), crs = 4326)
# sites_sf %>% leaflet() %>% addTiles() %>% addMarkers(popup = ~description)
