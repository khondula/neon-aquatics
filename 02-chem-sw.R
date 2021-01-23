# observational sampling data
# chemical properties of surface water
# DP1.20093.001

library(fs)
library(sf)
library(glue)

sites_df <- read_csv(glue('{data_dir}/field_sites.csv'))
unique(sites_df$field_site_type)
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

# get sampling location from field super parent


meta_files <- fs::dir_ls(glue('{chem_dir}/HOPB'), glob = "*SuperParent*")
hopb_df <- meta_files %>% purrr::map_df(~read_csv(.x))

lab_files <- fs::dir_ls(glue('{chem_dir}/HOPB'), glob = "*externalLab*")
hopb_chem_df <- lab_files %>% purrr::map_df(~read_csv(.x))
hopb_doc <- hopb_chem_df %>% dplyr::filter(analyte == 'DOC')
