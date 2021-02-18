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

get_abs_vals <- function(my_siteid){
  lab_files <- fs::dir_ls(glue('{chem_dir}/{my_siteid}'), glob = "*externalLab*")
  chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
  site_values <- chem_df %>% dplyr::filter(str_detect(analyte, 'Absorbance'))
  site_values_simp <- site_values %>% 
    dplyr::select(domainID, siteID, analyte) %>%
    distinct()
  return(site_values_simp)
}
get_abs_vals2 <- function(my_siteid){
  lab_files <- fs::dir_ls(glue('{chem_dir}/{my_siteid}'), glob = "*variables*")
  chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
  site_values <- chem_df %>% dplyr::filter(str_detect(fieldName, 'Absorbance'))
  site_values_simp <- site_values %>% 
    mutate(siteID = my_siteid) %>%
    dplyr::select(siteID, fieldName, siteID) %>%
    distinct()
  return(site_values_simp)
}

get_abs_vals2(aq_site_ids[1])

wavelengths_df2 <- aq_site_ids[1:10] %>% purrr::map_dfr(~get_abs_vals2(.x))
wavelengths_df3 <- aq_site_ids[11:20] %>% purrr::map_dfr(~get_abs_vals2(.x))
wavelengths_df4 <- aq_site_ids[21:34] %>% purrr::map_dfr(~get_abs_vals2(.x))

wavelengths_df4 %>% pull(fieldName) %>% unique()
wavelengths_df %>% write_csv('results/site-wavelengths.csv')
wavelengths_df2 %>% write_csv('results/site-wavelengths_vars.csv')
# 'UV Absorbance (250 nm)'

# get_values_site('HOPB', 'DOC')
suva280_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site(.x, my_analyte = 'UV Absorbance (280 nm)'))
suva250_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site(.x, my_analyte = 'UV Absorbance (250 nm)'))
doc_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site(.x, my_analyte = 'DOC'))
tss_df <- aq_site_ids %>% purrr::map_dfr(~get_values_site(.x, my_analyte = 'TSS'))

suva280_df %>% write_csv('results/suva280_all.csv')
suva_df %>% write_csv('results/suva250_all.csv')
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

