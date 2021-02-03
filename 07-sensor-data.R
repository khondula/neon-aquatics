# Get water quality
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(vroom)

wq_dir <- '~/Box/data/NEON/NEON_water-quality'
ts_dir <- '~/Box/data/NEON/wq-timeseries'

# try using Api to get IS water quality data
base_url <- 'http://data.neonscience.org/api/v0/'
data_id <- 'DP1.20288.001' # water quality
aq_site_ids <- read_lines('aq_site_ids.txt')
mysite <- aq_site_ids[1]

download_site_wq <- function(mysite){
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
  
  # filter to just the waq_instantaneous basic files
  get_waq_files <- function(my_url){
    data_files_req <- GET(my_url)
    data_files <- content(data_files_req, as = "text") %>% fromJSON()
    # future enhancement: check md5 sums for changes! 
    # compare to existing files!
    wq_files <- data_files$data$files$name %>% 
      fs::path_filter(regexp = "(waq_instantaneous).*(basic)")
    ids <- wq_files %>% purrr::map_int(~grep(.x, data_files$data$files$name))
    wq_files_urls <- data_files$data$files$url[ids]
    return(list(files = wq_files, urls = wq_files_urls))
  }
  
  my_files_list <- my_site_urls %>% purrr::map(~get_waq_files(.x))
  
  # download!
  fs::dir_create(glue('{wq_dir}/{mysite}'))
  # for each object in list my_files (each month-year)
  # download each of the files to local file
  
  download_month <- function(my_files){
    my_files_local <- glue('{wq_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  
}


rearrange_sensor_data <- function(mysite, wq_dir = '~/Box/data/NEON/NEON_water-quality'){
  # read all files in together
  wq_coltypes <- "TTddiddiddiddiddiddiddidddii"
  wq_df <- glue('{wq_dir}/{mysite}') %>%
    fs::dir_ls(glob = "*waq_instantaneous*") %>% # shouldnt need this row
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
  ts_dir <- '~/Box/data/NEON/wq-timeseries'
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
    site_ts_dir <- glue('{ts_dir}/{mysite}')
    fs::dir_create(site_ts_dir)
    filenames <- glue('{site_ts_dir}/{mysite}_{wq_param}_sensor{sensorids}.csv')
    wq_par_list %>% 
      purrr::walk2(.y = filenames, ~vroom_write(.x, .y, delim = ","))
    wq_par_list %>% purrr::map(~nrow(.x))
  }
  
  names(wq_params) %>% purrr::walk(~save_sensor_wq_timeseries(.x))
}

aq_site_ids

download_site_wq(mysite = "SYCA")
rearrange_sensor_data("SYCA")

download_site_wq(mysite = "CUPE")
rearrange_sensor_data("CUPE")

download_site_wq(mysite = "GUIL")
rearrange_sensor_data("GUIL")

download_site_wq(mysite = "KING")
rearrange_sensor_data("KING")

download_site_wq(mysite = "MCDI")
rearrange_sensor_data("MCDI")

download_site_wq(mysite = "LECO")
rearrange_sensor_data("LECO")

download_site_wq(mysite = "WALK")
rearrange_sensor_data("WALK")

download_site_wq(mysite = "MAYF")
rearrange_sensor_data("MAYF")

download_site_wq(mysite = "BLUE")
rearrange_sensor_data("BLUE")

download_site_wq(mysite = "PRIN")
rearrange_sensor_data("PRIN")

download_site_wq(mysite = "BLDE")
rearrange_sensor_data("BLDE")

download_site_wq(mysite = "WLOU")
rearrange_sensor_data("WLOU")

download_site_wq(mysite = "REDB")
rearrange_sensor_data("REDB")
