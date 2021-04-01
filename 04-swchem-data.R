# SW chem data

library(tidyverse)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)

data_dir <- '~/Box/data/NEON/spatial'
chem_dir <- '~/Box/data/NEON/NEON_chem-surfacewater'
source('R/myfxns.R')

# get number of DOC, TSS, chla samples per site

mysite <- aq_site_ids[3]

# Function to update chem values of surface water
mysite <- 'HOPB'
myglob <- 'fieldSuperParent'

update_swchem_files <- function(mysite, myglob){
  # current files
  chem_dir <- '~/Box/data/NEON/NEON_chem-surfacewater'
  lab_files <- fs::dir_ls(glue('{chem_dir}/{mysite}'), glob = glue("*{myglob}*"))
  sub1 <- nchar(basename(lab_files)[1])-33
  sub2 <- nchar(basename(lab_files)[1])-27
  months_have <- basename(lab_files) %>% str_sub(sub1, sub2)
  
  base_url <- 'http://data.neonscience.org/api/v0/'
  data_id <- 'DP1.20093.001' # surface water chem
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
  
  # filter to just the waq_instantaneous basic files
  # my_url <- my_site_urls[1]
  get_pattern_files <- function(my_url){
    data_files_req <- GET(my_url)
    data_files <- content(data_files_req, as = "text") %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
    data_files_df <- data_files$data$files %>% 
      filter(str_detect(name, glue('{myglob}.*(basic)')))
      # filter(str_detect(name, "(externalLabData).*(basic)"))
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
    my_files_local <- glue('{chem_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  if(any_new){message(glue('new {myglob} data from {mysite} for: {glue_collapse(months_newfiles, ", ")}'))}
  
}

update_swchem_files('ARIK', 'externalLabData')
# update_swchem_files(aq_site_ids[5])
aq_site_ids %>% purrr::walk(~update_swchem_files(.x, myglob = 'externalLabData'))
aq_site_ids %>% purrr::walk(~update_swchem_files(.x, myglob = 'fieldSuperParent'))
aq_site_ids %>% purrr::walk(~update_swchem_files(.x, myglob = 'swc_fieldData'))

##### once things are updated and downloaded

##### make a table of all sites and sampling dates
# maybe include whether or not data is downloaded if thats possible

### DATES ###
get_aos_dates <- function(mysite){
  fsp_files <- fs::dir_ls(glue('{chem_dir}/{mysite}'), glob = "*swc_fieldData*")
  fsp_df <- fsp_files %>% purrr::map_df(~read_csv(.x))
  fsp_df_sub <- fsp_df %>% 
    dplyr::select(domainID, siteID, namedLocation, collectDate, 
                sampleID, replicateNumber)
  field_sample_dates_df <- fsp_df_sub %>% 
    mutate(collect_date = lubridate::as_date(collectDate)) %>% 
    dplyr::select(domainID, siteID, namedLocation, collect_date) %>%
    distinct()
  return(field_sample_dates_df)
}

aos_dates_df <- aq_site_ids %>% purrr::map_dfr(~get_aos_dates(.x))
aos_dates_df %>% write_csv('results/all-aos-dates.csv')

### SW CHEM DATA ####
get_values_site_swchem <- function(my_siteid, my_analyte){
  chem_dir <- '~/Box/data/NEON/NEON_chem-surfacewater'
  lab_files <- fs::dir_ls(glue('{chem_dir}/{my_siteid}'), glob = "*externalLab*")
  chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
  site_values <- chem_df %>% dplyr::filter(analyte == my_analyte)
  site_values_simp <- site_values %>% 
    dplyr::select(uid, domainID, siteID, namedLocation, collectDate,
                  analyte, analyteConcentration, analyteUnits, sampleCondition)
  return(site_values_simp)
}

# getting absorbance wavelengths
get_abs_vals <- function(my_siteid){
  lab_files <- fs::dir_ls(glue('{chem_dir}/{my_siteid}'), glob = "*externalLab*")
  chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
  site_values <- chem_df %>% dplyr::filter(str_detect(analyte, 'Absorbance'))
  site_values_simp <- site_values %>% 
    dplyr::select(domainID, siteID, analyte) %>%
    distinct()
  return(site_values_simp)
}

wavelengths_df4 %>% pull(fieldName) %>% unique()
wavelengths_df %>% write_csv('results/site-wavelengths.csv')
wavelengths_df2 %>% write_csv('results/site-wavelengths_vars.csv')
# 'UV Absorbance (250 nm)'

# get_values_site('HOPB', 'DOC')
suva280_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site_swchem(.x, my_analyte = 'UV Absorbance (280 nm)'))
suva254_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site_swchem(.x, my_analyte = 'UV Absorbance (250 nm)'))
doc_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site_swchem(.x, my_analyte = 'DOC'))
tss_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site_swchem(.x, my_analyte = 'TSS'))

suva280_df %>% write_csv('results/suva280_all.csv')
suva254_df %>% write_csv('results/suva254_all.csv')
doc_df %>% write_csv('results/doc_all.csv')
tss_df %>% write_csv('results/tss_all.csv')

# chlorophyll a data from... 

# how many samples per named location?
doc_coltypes <- 'ccccTcdcc'
doc_df <- read_csv('results/doc_all.csv', col_types = doc_coltypes) %>%
  mutate(date = lubridate::as_date(collectDate))

doc_summary <- doc_df %>% 
  group_by(siteID, namedLocation) %>% 
  dplyr::filter(!is.na(analyteConcentration)) %>%
  summarise(n_doc = n(), 
            min_date = as_date(min(collectDate)),
            max_date = as_date(max(collectDate)),
            median_doc_mgL = median(analyteConcentration),
            min_doc_mgL = min(analyteConcentration),
            max_doc_mgL = max(analyteConcentration)) 

doc_summary %>% write_csv('results/doc-overview')

