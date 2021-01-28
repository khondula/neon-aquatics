# SW chem data

library(tidyverse)
library(fs)
library(glue)
library(sf)

data_dir <- '~/Box/data/NEON/spatial'
chem_dir <- '~/Box/data/NEON/NEON_chem-surfacewater'
sites_df <- read_csv(glue('{data_dir}/field_sites.csv'))
aq_site_ids <- sites_df %>% 
  dplyr::filter(field_site_type %in% 
                  c("Relocatable Aquatic", "Core Aquatic")) %>% 
  pull(field_site_id)

# get number of DOC, TSS, chla samples per site

my_siteid <- 'HOPB'
my_analyte <- 'DOC'

get_values_site <- function(my_siteid, my_analyte){
  lab_files <- fs::dir_ls(glue('{chem_dir}/{my_siteid}'), glob = "*externalLab*")
  chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
  site_values <- chem_df %>% dplyr::filter(analyte == my_analyte)
  site_values_simp <- site_values %>% 
    dplyr::select(uid, domainID, siteID, namedLocation, collectDate,
                  analyte, analyteConcentration, analyteUnits, sampleCondition)
  return(site_values_simp)
}

# 'UV Absorbance (250 nm)'

# get_values_site('HOPB', 'DOC')
doc_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site(.x, my_analyte = 'DOC'))
tss_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site(.x, my_analyte = 'TSS'))

doc_df %>% write_csv('results/doc_all.csv')
tss_df %>% write_csv('results/tss_all.csv')

# chlorophyll a data from... 
                                        
