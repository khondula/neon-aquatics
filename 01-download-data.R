library(sf)
library(tidyverse)
library(raster)
library(fs)
library(glue)

data_dir <- '~/Box/data/NEON/spatial'

# shapefiles from internet
aop_boxes_url <- 'https://neon.maps.arcgis.com/sharing/rest/content/items/f27616de7f9f401b8732cdf8902ab1d8/data'
watersheds_url <- 'https://neon.maps.arcgis.com/sharing/rest/content/items/869c18de0c874c33b352efad0778a07a/data'
reaches_url <- 'https://neon.maps.arcgis.com/sharing/rest/content/items/2391e7b863d74afcb066401224e28552/data'
domains_url <- 'https://neon.maps.arcgis.com/sharing/rest/content/items/e45d2bf677e245488a201afe02f1ad74/data'

get_spatial <- function(myname, myurl){
  localzip <- glue('{data_dir}/{myname}.zip')
  download.file(myurl, localzip)
  unzip(localzip, exdir = glue('{data_dir}/{myname}/'))
  fs::file_delete(localzip)
  message(glue('{myname} files downloaded'))
}

get_spatial('aop_boxes', aop_boxes_url)
get_spatial('watersheds', watersheds_url)
get_spatial('reaches', reaches_url)
get_spatial('domains', domains_url)

# field sites table
field_sites_url <- 'https://www.neonscience.org/sites/default/files/NEON_Field_Site_Metadata_20201204.csv'
download.file(field_sites_url, glue('{data_dir}/field_sites.csv'))

# water chem data
sites_df <- read_csv(glue('{data_dir}/field_sites.csv'))
aq_site_ids <- sites_df %>% dplyr::filter(field_site_type %in% c("Relocatable Aquatic", "Core Aquatic")) %>% pull(field_site_id)

# downloaded all sw chem data to:
chem_dir <- '~/Documents/data/NEON/NEON_chem-surfacewater'

unzip_by_site <- function(siteid){
  myfiles <- fs::dir_ls(chem_dir, glob = glue("*{siteid}*")) 
  n_files <- length(myfiles)
  months <- basename(myfiles) %>% substr(29, 35) %>% sort()
  myfiles %>% purrr::walk(~unzip(.x, exdir = glue('{chem_dir}/{siteid}')))
  fs::file_delete(myfiles)
  message(glue('{n_files} files from {siteid} between {head(months, 1)} and {tail(months, 1)}'))
}

aq_site_ids %>% purrr::walk(~unzip_by_site(.x))

# water quality sensor data
# downloaded everything for COMO to test
wq_dir <- '~/Box/data/NEON/NEON_water-quality'
siteid <- 'COMO'

# move all data from a site into one folder with site name
mydirs <- fs::dir_ls(wq_dir, glob = glue('*{siteid}*'))
myfiles <- fs::dir_ls(mydirs, recurse = 1)
fs::dir_create(glue('{wq_dir}/{siteid}'))
myfiles_new <- myfiles %>% map_chr(~glue('{wq_dir}/{siteid}/{basename(.x)}'))
purrr::walk2(.x = myfiles, .y = myfiles_new, ~fs::file_move(.x, .y))
fs::dir_delete(mydirs)

