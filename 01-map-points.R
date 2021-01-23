library(sf)
library(tidyverse)
library(raster)
library(fs)
library(glue)

data_dir <- '~/Box/data/NEON/spatial'
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

field_sites_url <- 'https://www.neonscience.org/sites/default/files/NEON_Field_Site_Metadata_20201204.csv'
download.file(field_sites_url, glue('{data_dir}/field_sites.csv'))
