library(tidyverse)
library(fs)
library(glue)
library(sf)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)

positions_dir <- '~/Box/data/NEON/spatial/suna-sensor-positions'
fs::dir_create(positions_dir)
aq_site_ids <- read_lines('aq_site_ids.txt')

mysite <- aq_site_ids[1]

# get all sensor positions for a site with API

base_url <- 'http://data.neonscience.org/api/v0/'
# data_id <- 'DP1.20288.001' # water quality
data_id <- 'DP1.20033.001' # nitrate in surface water

# NEED base_url, data_id
save_sensor_positions <- function(mysite){

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
    # add line here to filter for filter dates after
    # what is already downloaded?
    pull(url)
  
  # my_url <- my_site_urls[1]
  # filter to just the waq_instantaneous basic files
  # or whatever file type is of interest
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
  
  # then get file locations
  my_files_list <- my_site_urls %>% 
    purrr::map(~get_positions_files(.x))
  fs::dir_create(glue('{positions_dir}/{mysite}'))
  
  # function to download
  download_month <- function(my_files){
    my_files_local <- glue('{positions_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  # download
  my_files_list %>% purrr::walk(~download_month(.x))
  ##
  
  coords_df <- glue('{positions_dir}/{mysite}') %>%
    fs::dir_ls(glob = "*sensor_positions*") %>% 
    # purrr::map_dfr(~read_csv(.x, col_types = c("dccTlccTlddddddddd"))) %>%
    purrr::map_dfr(~read_csv(.x)) %>%
    dplyr::select(HOR.VER, name, description, xOffset, yOffset, zOffset, 
                  referenceLatitude, referenceLongitude, referenceElevation) %>%
    distinct()
  
  glue('{positions_dir}/{mysite}') %>%
    fs::dir_ls() %>% purrr::walk(~fs::file_delete(.x))
  filename <- glue('{positions_dir}/{mysite}/{mysite}_SUNA-sensor-positions.csv')
  coords_df %>% write_csv(filename)
  
}

# aq_site_ids[2] %>% purrr::walk(~save_sensor_positions(.x))
# aq_site_ids[31:34] %>% purrr::walk(~save_sensor_positions(.x))
# assume all lats and long in crs 4326?
# what units are offset in? 

# read in sensor position files
suna_positions_df <- glue('{positions_dir}') %>%
  fs::dir_ls(glob = "*SUNA-sensor-positions*", recurse = 1) %>%
  purrr::map_df(~read_csv(.x), .id = 'filename') %>%
  mutate(filename = basename(filename)) %>%
  mutate(siteid = substr(filename, 1, 4)) %>%
  mutate(sensor_position = substr(HOR.VER, 1, 3)) %>%
  dplyr::select(siteid, sensor_position, name, description) %>%
  distinct()

# THEN, read in AOP flight dates with aquatic site ids 
aq_aop_dates <- read_csv('results/aquatic-sites-aop-dates.csv') %>%
  rename(aop_siteid = siteid)
# and join
suna_x_aop_df <- suna_positions_df %>%
  left_join(aq_aop_dates, by = c('siteid' = 'siteID'))
# domain, site id, aop site, sensor position, filght date
suna_x_aop_df %>% 
  dplyr::select(domanID, domainName, siteid, description, 
                aop_siteid, YearSiteVisit, year, flightdate, sensor_position) %>%
  write_csv('results/suna-x-aop.csv')

sites_sf <- coords_df %>% sf::st_as_sf(coords = c("referenceLongitude", "referenceLatitude"), crs = 4326)
# sites_sf %>% leaflet() %>% addTiles() %>% addMarkers(popup = ~description)
