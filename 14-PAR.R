# par just above and below water surface
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(vroom)
source('R/myfxns.R')

par_above_dir <- '~/Box/data/NEON/NEON_par-water-surface'

# mysite <- aq_site_ids[1]
mysite <- 'BARC'
# get all sensor positions for a site with API
download_site_par <- function(mysite){
  base_url <- 'http://data.neonscience.org/api/v0/'
  data_id_above <- 'DP1.20042.001' # at water surface

  data_id <- data_id_above
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
  
  # filter to just the {myglob} basic files
  # my_url <- my_site_urls[1]
  get_PAR_files <- function(my_url){
    data_files_req <- GET(my_url)
    data_files <- content(data_files_req, as = "text") %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
    data_files_df <- data_files$data$files %>% 
      filter(str_detect(name, "(PARWS_30min).*(basic)"))
    # future enhancement: check md5 sums for changes! 
    # compare to existing files!
    return(list(files = data_files_df$name, urls = data_files_df$url))
  }
  
  my_files_list <- my_site_urls %>% purrr::map(~get_PAR_files(.x))
  
  # download!
  fs::dir_create(glue('{par_above_dir}/{mysite}'))
  # for each object in list my_files (each month-year)
  # download each of the files to local file
  
  download_month <- function(my_files){
    my_files_local <- glue('{par_above_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  
}

