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
aop_boxes <- st_read(glue('{data_dir}/aop_boxes/AOP_flightboxesAllSites.shp'))

sites_sf %>% leaflet() %>% addTiles() %>% 
  addMarkers(popup = ~namedLocation) %>%
  addPolygons(data = aop_boxes, popup = ~flightbxID)

# aop_boxes have different names for aq and terrestrial but same box ID
# 99 rows in flight boxes data, 70 unique IDs
aop_boxes$flightbxID %>% unique() %>% length()

aop_boxes_unique <- aop_boxes %>% 
  arrange(domain, sampleType) %>%
  group_by(domain, domainName, flightbxID) %>% distinct()

sites_x_aop <- sites_sf %>% st_join(aop_boxes_unique, left = TRUE)
sites_x_aop %>% write_csv('results/sites_x_aop.csv')
# BLUE has 2 overlapping flight boxes, one small one big
# REDB AOS S2 location has 2 overlapping points

# actual values in here
# lab_files <- fs::dir_ls(glue('{chem_dir}/HOPB'), glob = "*externalLab*")
# hopb_chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
# hopb_doc <- hopb_chem_df %>% dplyr::filter(analyte == 'DOC')
