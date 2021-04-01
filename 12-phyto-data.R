# phytoplankton biomass data
library(tidyverse)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)
library(hms)

# data_dir <- '~/Box/data/NEON/spatial'
phyto_dir <- '~/Box/data/NEON/NEON_chem-peri-ses-phyto'
phyto_dir2 <- '~/Box/data/NEON/chl-a'
source('R/myfxns.R')

mysite <- aq_site_ids[4]
# myglob <- 'algaeExternalLabDataPerSample'

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

update_phyto_files('ARIK', 'alg_fieldData')
update_phyto_files('ARIK', 'algaeExternalLabDataPerSample')
aq_site_ids %>% purrr::walk(~update_phyto_files(.x, 'alg_fieldData'))
aq_site_ids %>% purrr::walk(~update_phyto_files(.x, 'algaeExternalLabDataPerSample'))

# join the field and lab data files 
# by site-months
join_field_and_lab <- function(mysite){
  glue('{phyto_dir}/{mysite}') %>% fs::dir_create()
  lab_files <- fs::dir_ls(glue('{phyto_dir}/{mysite}'), glob = glue("*algaeExternalLabDataPerSample*"))
  field_files <- fs::dir_ls(glue('{phyto_dir}/{mysite}'), glob = glue("*alg_fieldData*"))
  sub1 <- nchar(basename(lab_files)[1])-33
  sub2 <- nchar(basename(lab_files)[1])-27
  months_have <- basename(lab_files) %>% str_sub(sub1, sub2)
  # my_month <- months_have[1]
  join_month <- function(my_month){
    lab_df <- grep(my_month, lab_files, value = TRUE) %>% 
      read_csv() %>% 
      dplyr::select(domainID, siteID, namedLocation, collectDate,
                    sampleID, sampleCondition, replicate,
                    analyte, analyteConcentration, plantAlgaeLabUnits,
                    externalLabDataQF)
    field_cols <- 'cccccdddddcTcccccccccccccccccc'
    field_df <- grep(my_month, field_files, value = TRUE) %>% 
      read_csv(col_types = field_cols) %>% 
      mutate(time_hms = as_hms(collectDate),
             collect_date = as_date(collectDate)) %>%
      dplyr::select(namedLocation, aquaticSiteType, collect_date,
                    time_hms, parentSampleID, habitatType,
                    algalSampleType, phytoDepth1,
                    phytoDepth2, phytoDepth3, substratumSizeClass)
    
    # make table between sampleId and parentSampleID
    # based on field table
    my_lookups <- field_df$parentSampleID %>%
      set_names(field_df$parentSampleID) %>%
      purrr::map(~grep(.x, lab_df$sampleID, value = TRUE)) %>%
      purrr::map_df(~as_tibble(.x), .id = 'parentSampleID') %>%
      rename(sampleID = value) %>% 
      distinct()

    lab_join_field <- lab_df %>% 
      left_join(my_lookups, by = 'sampleID') %>%
      left_join(field_df, by = c('parentSampleID', 'namedLocation')) 
    
    return(lab_join_field)
  }
  all_lab_join_field <- months_have %>% purrr::map_dfr(~join_month(.x))
  all_lab_join_field %>%
    write_csv(glue('{phyto_dir2}/{mysite}_phyto-chem.csv'))
}
  
aq_site_ids %>% purrr::walk(~join_field_and_lab(.x))

# just because there is a file doesnt mean 
# that there is chl a data... 

mysite <- 'POSE'
my_analytes <- c('chlorophyll a', 'pheophytin', 'total cholorophyll a')

get_values_site <- function(mysite, my_analyte){
  my_files <- fs::dir_ls(phyto_dir2, glob = glue("*{mysite}*"))
  # chla_coltypes <- 'cccDtcccdcccccidddc'
  chem_df <- my_files %>% purrr::map_df(~read_csv(.x))
  site_values <- chem_df %>% dplyr::filter(analyte %in% my_analytes)
  site_values_simp <- site_values %>% 
    dplyr::filter(!is.na(analyteConcentration))
  return(site_values_simp)
}

get_values_site('ARIK', my_analytes)
phyto_df <- aq_site_ids %>% 
  purrr::map_dfr(~get_values_site(.x, my_analytes))

phyto_df$algalSampleType %>% table()
phyto_summary <- phyto_df %>%
  dplyr::filter(algalSampleType %in% c('seston', 'phytoplankton')) %>%
  group_by(domainID, siteID, namedLocation, collectDate, analyte, algalSampleType) %>%
  summarise(n = n()) %>% 
  tidyr::pivot_wider(names_from = analyte, values_from = n)
phyto_summary %>% write_csv('results/phyto-data-overview.csv')

# maybe add in dates to when you are saving these files
phyto_df %>% write_csv('results/all-phyto-data.csv')



