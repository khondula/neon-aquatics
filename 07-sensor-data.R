# Get water quality
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
# library(lubridate)
# library(vroom)

# try using Api to get IS water quality data
base_url <- 'http://data.neonscience.org/api/v0/'
data_id <- 'DP1.20288.001' # water quality

req_avail <- GET(glue('{base_url}/products/{data_id}'))
avail_resp <- content(req_avail, as = 'text') %>% 
  fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

# List of products by site code with month
# eg ARIK/2016-03
data_urls_list <- avail_resp$data$siteCodes$availableDataUrls
# names(data_urls_list) <- avail_resp$data$siteCodes$siteCode
# data_urls <- data_urls_list %>% unlist()

# make this into a table
avail_df <- data_urls_list %>%
  unlist() %>% as.data.frame() %>%
  dplyr::rename(url = 1) %>%
  mutate(siteid = str_sub(url, 56, 59)) %>%
  mutate(month = str_sub(url, 61, 67)) %>%
  dplyr::select(siteid, month, url)

my_url <- avail_df$url[100]
# actual files available
# get_img_datetimes <- function(my_url){
  data_files_req <- GET(my_url)
  data_files <- content(data_files_req, as = "text") %>% fromJSON()
  # filter to just the waq_instantaneous basic files
  # future enhancement: check md5 sums for changes! 
  wq_files <- data_files$data$files$name %>% 
    fs::path_filter(regexp = "(waq_instantaneous).*(basic)")
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



# test out with COMO data
wq_dir <- '~/Box/data/NEON/NEON_water-quality'
ts_dir <- '~/Box/data/NEON/wq-timeseries'
# pull in data for one site (COMO)
# AFTER moved all files to folder in 01

# water quality sensor data
# downloaded everything for one site to test
wq_dir <- '~/Box/data/NEON/NEON_water-quality 2'
siteid <- 'ARIK'

# first move all data from a site into one folder with site name

rearrange_sensor_data <- function(wq_dir, siteid){
  # rearrange downloaded files into one directory
  mydirs <- fs::dir_ls(wq_dir, glob = glue('*{siteid}*'))
  myfiles <- fs::dir_ls(mydirs, recurse = 1)
  fs::dir_create(glue('{wq_dir}/{siteid}'))
  myfiles_new <- myfiles %>% map_chr(~glue('{wq_dir}/{siteid}/{basename(.x)}'))
  purrr::walk2(.x = myfiles, .y = myfiles_new, ~fs::file_move(.x, .y))
  fs::dir_delete(mydirs)
  
  # then read all files in together
  wq_coltypes <- "TTddiddiddiddiddiddiddidddii"
  wq_df <- glue('{wq_dir}/{siteid}') %>%
    fs::dir_ls(glob = "*waq_instantaneous*") %>% 
    purrr::map_dfr(~read_csv(.x, col_types = wq_coltypes), .id = 'filename') %>%
    dplyr::mutate(filename = basename(filename)) %>% 
    dplyr::mutate(siteid = substr(filename, 10, 13),
                  sensor_position = substr(filename, 29, 33))
  
  # then save separate time series for each sensor
  # SC, DO, DOsat, pH, chl, turb, fDOM
  wq_params <- list(specificConductance = names(wq_df)[7:9],
                    dissolvedOxygen = names(wq_df)[10:12],
                    dissolvedOxygenSaturation = names(wq_df)[13:15],
                    pH = names(wq_df)[16:18],
                    chlorophyll = names(wq_df)[19:21],
                    turbidity = names(wq_df)[22:24],
                    fDOM = names(wq_df)[25:28])
  
  #  then save separate CSV for each parameter and sensor
  # need to have wq_df, wq_params, ts_dir, siteid
  save_sensor_wq_timeseries <- function(wq_param){
    # list of parameter data for each sensor
    wq_par_list <- wq_df %>% 
      filter(!is.na(wq_params[[wq_param]][1])) %>%
      dplyr::select(siteid, sensor_position, startDateTime, 
                    endDateTime, wq_params[[wq_param]]) %>%
      group_by(sensor_position) %>% group_split()
    # name list with each sensor
    sensorids <- wq_par_list %>% 
      purrr::map_chr(~unique(pull(.x, sensor_position)))
    names(wq_par_list) <- sensorids
    # make directory and save files
    site_ts_dir <- glue('{ts_dir}/{siteid}')
    fs::dir_create(site_ts_dir)
    filenames <- glue('{site_ts_dir}/{siteid}_{wq_param}_sensor{sensorids}.csv')
    wq_par_list %>% 
      purrr::walk2(.y = filenames, ~vroom_write(.x, .y, delim = ","))
    wq_par_list %>% purrr::map(~nrow(.x))
  }
  
  names(wq_params) %>% purrr::walk(~save_sensor_wq_timeseries(.x))
  # delete files for site
  fs::dir_delete(glue('{wq_dir}/{siteid}'))
}


rearrange_sensor_data(wq_dir = "~/Box/data/NEON/NEON_water-quality", "BIGC")
rearrange_sensor_data(wq_dir = "~/Box/data/NEON/NEON_water-quality", "BARC")


# wq_df %>%
#   ggplot(aes(x = startDateTime, y = fDOM))
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


