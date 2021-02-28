# phytoplankton biomass data
library(tidyverse)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)

# data_dir <- '~/Box/data/NEON/spatial'
phyto_dir <- '~/Box/data/NEON/NEON_chem-peri-ses-phyto'
source('R/myfxns.R')

mysite <- aq_site_ids[3]
myglob <- 'algaeExternalLabDataPerSample'

update_phyto_files <- function(mysite, myglob){
  # current files
  glue('{phyto_dir}/{mysite}') %>% fs::dir_create()
  lab_files <- fs::dir_ls(glue('{phyto_dir}/{mysite}'), glob = glue("*{myglob}*"))
  sub1 <- nchar(basename(lab_files)[1])-33
  sub2 <- nchar(basename(lab_files)[1])-27
  months_have <- basename(lab_files) %>% str_sub(sub1, sub2)
  
  base_url <- 'http://data.neonscience.org/api/v0/'
  data_id <- 'DP1.20163.001' # phytoplankton chem
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
  
  my_site_to_get <- avail_df %>% 
    dplyr::filter(siteid == mysite) %>%
    dplyr::filter(!month %in% months_have) %>%
    dplyr::filter(as.numeric(substr(month, 1, 4)) > 2011)
  
  my_site_urls <- my_site_to_get %>% pull(url)
  
  # filter to just the {myglob} basic files
  # my_url <- my_site_urls[1]
  get_pattern_files <- function(my_url){
    data_files_req <- GET(my_url)
    data_files <- content(data_files_req, as = "text") %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
    data_files_df <- data_files$data$files %>% 
      filter(str_detect(name, glue('{myglob}.*(basic)')))
    # future enhancement: check md5 sums for changes! 
    return_list <- NULL
    if(nrow(data_files_df) > 0){
      return_list <- list(files = data_files_df$name, urls = data_files_df$url)}
    return(return_list)
  }
  
  my_files_list <- my_site_urls %>% purrr::map(~get_pattern_files(.x))
  
  new_files <- my_files_list %>% map_lgl(~!is.null(.x))
  any_new <- any(new_files)
  if(!any_new){message(glue('No new {myglob} data from {mysite}'))}
  months_newfiles <- my_site_to_get[['month']][which(new_files)]
  download_month <- function(my_files){
    my_files_local <- glue('{phyto_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  if(any_new){message(glue('new {myglob} data from {mysite} for: {glue_collapse(months_newfiles, ", ")}'))}
  
}

update_phyto_files('ARIK', 'algaeExternalLabDataPerSample')

