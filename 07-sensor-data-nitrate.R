# Get water quality
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(vroom)

suna_dir <- '~/Box/data/NEON/NEON_nitrate'
suna_ts_dir <- '~/Box/data/NEON/suna-timeseries'
fs::dir_create(suna_dir)
fs::dir_create(suna_ts_dir)

# try using Api to get IS water quality data
# base_url <- 'http://data.neonscience.org/api/v0/'
# data_id <- 'DP1.20288.001' # water quality
aq_site_ids <- read_lines('aq_site_ids.txt')
mysite <- aq_site_ids[1]

download_site_suna <- function(mysite){
  base_url <- 'http://data.neonscience.org/api/v0/'
  data_id <- 'DP1.20033.001' # nitrate in surface water
  # data_id <- 'DP1.20288.001' # water quality
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
    data_files <- content(data_files_req, as = "text") %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
    data_files_df <- data_files$data$files %>% 
      filter(str_detect(name, "(NSW_15_minute).*(basic)"))
    # future enhancement: check md5 sums for changes! 
    # compare to existing files!
    return(list(files = data_files_df$name, urls = data_files_df$url))
  }
  
  my_files_list <- my_site_urls %>% purrr::map(~get_waq_files(.x))
  
  # download!
  fs::dir_create(glue('{suna_dir}/{mysite}'))
  # for each object in list my_files (each month-year)
  # download each of the files to local file
  
  download_month <- function(my_files){
    my_files_local <- glue('{suna_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  
}


rearrange_sensor_data_nitrate <- function(mysite, suna_dir = '~/Box/data/NEON/NEON_nitrate'){
  # read all files in together
  suna_coltypes <- "TTdddddddi"
  suna_df <- glue('{suna_dir}/{mysite}') %>%
    fs::dir_ls(glob = "*NSW_15_minute*") %>%
    purrr::map_dfr(~read_csv(.x, col_types = suna_coltypes), .id = 'filename') %>%
    dplyr::mutate(filename = basename(filename)) %>% 
    dplyr::mutate(siteid = substr(filename, 10, 13),
                  sensor_position = substr(filename, 29, 31))
  
  # then save separate time series for each sensor
  #  then save separate CSV for each parameter and sensor
  # need to have wq_df, wq_params, ts_dir, siteid
  suna_ts_dir <- '~/Box/data/NEON/suna-timeseries'
  # save_suna_timeseries <- function(wq_param){
    # list of parameter data for each sensor
    suna_list <- suna_df %>% 
      dplyr::select(siteid, sensor_position, startDateTime, 
                    endDateTime, surfWaterNitrateMean, finalQF) %>%
      group_by(sensor_position) %>% group_split()
    # name list with each sensor
    sensorids <- suna_list %>% 
      purrr::map_chr(~unique(pull(.x, sensor_position)))
    names(suna_list) <- sensorids
    ## HERE -- check for any non NA values before saving? ##
    # make directory and save files
    site_ts_dir <- glue('{suna_ts_dir}/{mysite}')
    fs::dir_create(site_ts_dir)
    filenames <- glue('{site_ts_dir}/{mysite}_SUNA_sensor{sensorids}.csv')
    suna_list %>% 
      purrr::walk2(.y = filenames, ~vroom_write(.x, .y, delim = ","))
  
}

# computer says please dont do all at the same time 
download_site_suna(aq_site_ids[2])
rearrange_sensor_data_nitrate(aq_site_ids[2])

# aq_site_ids[31:34] %>% purrr::walk(~download_site_suna(.x))
# aq_site_ids[31:34] %>% purrr::walk(~rearrange_sensor_data_nitrate(.x))

# Remove files that have no non-NA values
library(data.table)

SUNA_files <- fs::dir_ls(suna_ts_dir, recurse = 1, regexp = 'SUNA')
SUNA_file <- SUNA_files[1]

check_suna_nrows <- function(SUNA_file){
  my_dt <- fread(SUNA_file)
  my_nrow <- nrow(my_dt[!is.na(surfWaterNitrateMean)])
  if(my_nrow<1){
    fs::file_delete(SUNA_file)
    message(glue("No data in {basename(SUNA_file)}, deleting"))}
}

SUNA_files <- fs::dir_ls(suna_ts_dir, recurse = 1, regexp = 'SUNA')
SUNA_files %>% purrr::walk(~check_suna_nrows(.x))


