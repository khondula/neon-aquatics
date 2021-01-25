# observational sampling data
# chemical properties of surface water
# DP1.20093.001

library(tidyverse)
library(fs)
library(glue)
library(sf)

data_dir <- '~/Box/data/NEON/spatial'
chem_dir <- '~/Documents/data/NEON/NEON_chem-surfacewater'
sites_df <- read_csv(glue('{data_dir}/field_sites.csv'))
aq_site_ids <- sites_df %>% dplyr::filter(field_site_type %in% c("Relocatable Aquatic", "Core Aquatic")) %>% pull(field_site_id)

# get sampling location for from field super parent
get_swchem_sites <- function(siteid){
  coords <- fs::dir_ls(glue('{chem_dir}/{siteid}'), glob = "*SuperParent*") %>% 
    purrr::map_df(~read_csv(.x)) %>% 
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, geodeticDatum) %>%
    distinct()
  return(coords)
}

# make and save table with coords of all sampling locations (n = 74)
swchem_sites_df <- aq_site_ids %>% purrr::map_df(~get_swchem_sites(.x))
swchem_sites_df %>% write_csv('results/swchem_sites_df.csv')

# save as shapefile
sites_sf <- swchem_sites_df %>% sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
sites_dir <- glue('{data_dir}/swchem_sites')
if(!dir_exists(sites_dir)){fs::dir_create(sites_dir)}
sites_sf %>% st_write(glue('{sites_dir}/swchem_sites.shp'))

sites_sf <- st_read(glue('{sites_dir}/swchem_sites.shp')) %>% rename(namedLocation = 3)

# read in AOP locations
sites_sf %>% leaflet() %>% addTiles() %>% addMarkers(popup = ~namedLocation)
# actual values in here
# lab_files <- fs::dir_ls(glue('{chem_dir}/HOPB'), glob = "*externalLab*")
# hopb_chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
# hopb_doc <- hopb_chem_df %>% dplyr::filter(analyte == 'DOC')
