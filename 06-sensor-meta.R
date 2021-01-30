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

wq_dir <- '~/Box/data/NEON/NEON_water-qualityty'
siteid <- 'COMO'

fs::dir_ls(glue('{wq_dir}/{siteid}'), glob = "*sensor_positions*", recurse = 1) %>% 
  purrr::map_df(~read_csv(.x))