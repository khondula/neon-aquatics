library(tidyverse)
library(fs)
library(glue)
library(sf)

data_dir <- '~/Box/data/NEON/spatial'

# sites_df <- read_csv(glue('{data_dir}/field_sites.csv'))
# aq_site_ids <- sites_df %>% 
#   dplyr::filter(field_site_type %in% 
#                   c("Relocatable Aquatic", "Core Aquatic")) %>% 
#   pull(field_site_id)

wq_dir <- '~/Box/data/NEON/NEON_water-quality'
siteid <- 'COMO'

# get all sensor positions for a site
# woops deleted this folder

coords_df <- glue('{wq_dir}/{siteid}') %>%
  fs::dir_ls(glob = "*sensor_positions*", recurse = 1) %>% 
  purrr::map_dfr(~read_csv(.x, col_types = c("dccTlccTlddddddddd"))) %>%
  dplyr::select(HOR.VER, name, description, xOffset, yOffset, zOffset, 
                referenceLatitude, referenceLongitude, referenceElevation) %>%
  distinct()

head(coords_df)

## TODO: function to get sensor positions for all aq sites ##
# assume all lats and long in crs 4326?
# what units are offset in? 

sites_sf <- coords_df %>% sf::st_as_sf(coords = c("referenceLongitude", "referenceLatitude"), crs = 4326)
# sites_sf %>% leaflet() %>% addTiles() %>% addMarkers(popup = ~description)
