# phyto taxonomy
library(cowplot)
library(tidyverse)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)
library(hms)

taxo_dir <- '~/Box/data/NEON/NEON_peri-ses-phyto'
source('R/myfxns.R')

mysite <- aq_site_ids[4]
myglob <- 'alg_taxonomyProcessed'

update_taxo_files <- function(mysite, myglob){
  # current files
  glue('{taxo_dir}/{mysite}') %>% fs::dir_create()
  if(length(fs::dir_ls(taxo_dir))>0){
    my_files <- fs::dir_ls(glue('{taxo_dir}/{mysite}'), glob = glue("*{myglob}*"))
    sub1 <- nchar(basename(my_files)[1])-33
    sub2 <- nchar(basename(my_files)[1])-27
    months_have <- basename(my_files) %>% str_sub(sub1, sub2)
    }
  
  base_url <- 'http://data.neonscience.org/api/v0/'
  data_id <- 'DP1.20166.001' # phytoplankton taxonomy
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
  
  # filter to just the {myglob} basic files
  # my_url <- my_site_urls[1]
  get_pattern_files <- function(my_url){
    data_files_req <- GET(my_url)
    data_files <- content(data_files_req, as = "text") %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
    data_files_df <- data_files$data$files %>% 
      filter(str_detect(name, glue('{myglob}.*(basic)')))
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
    my_files_local <- glue('{taxo_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  if(any_new){message(glue('new {myglob} data from {mysite} for: {glue_collapse(months_newfiles, ", ")}'))}
  
}

update_taxo_files('ARIK', 'alg_taxonomyProcessed')
aq_site_ids %>% purrr::walk(~update_taxo_files(.x, 'alg_taxonomyProcessed'))


### summarize taxo data by sample ###
mysite <- 'BLWA'
  
taxo_files <- fs::dir_ls(glue('{taxo_dir}/{mysite}'), glob = "*alg_taxonomyProcessed*")
taxo_df <- taxo_files %>% purrr::map_df(~read_csv(.x))

taxo_df_biovolume <- taxo_df %>% 
  dplyr::filter(algalParameter == 'biovolume density') %>%
  group_by(domainID, siteID, namedLocation, collectDate, sampleID, algalType, division, class) %>%
  summarise(n_taxa = n(), class_biovolume = sum(algalParameterValue))

sample_biovol_df <- taxo_df_biovolume %>%
  group_by(domainID, siteID, namedLocation, collectDate, sampleID) %>%
  summarise(sample_biovolume = sum(class_biovolume))

taxo_df_biovolume_join <- taxo_df_biovolume %>% 
  left_join(sample_biovol_df) %>% 
  mutate(class_biovol_prop = class_biovolume/sample_biovolume) %>%
  mutate(sampleID_short = str_remove_all(sampleID, pattern = glue('{mysite}|TAXONOMY|[[:punct:]]'))) %>%
  mutate(location_type = str_sub(namedLocation, 10, nchar(namedLocation)))

# x axis labels should be just date of sample
aa <- taxo_df_biovolume_join %>%
  dplyr::filter(str_detect(namedLocation, 'riparian', TRUE)) %>%
  ggplot(aes(x = sampleID_short, y = class_biovolume, fill = class)) +
  geom_bar(stat = 'identity', color = 'black', lwd = 0.1) +
  facet_wrap(vars(location_type), scale = 'free', ncol = 1) +
  theme_minimal() + coord_flip() +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ggtitle(glue('{mysite} biovolume by class'))

bb <- taxo_df_biovolume_join %>%
  dplyr::filter(str_detect(namedLocation, 'riparian', TRUE)) %>%
  ggplot(aes(x = sampleID_short, y = class_biovol_prop, fill = class)) +
  geom_bar(stat = 'identity', color = 'black', lwd = 0.2) +
  facet_wrap(vars(location_type), ncol = 1, scales = 'free_y') +
  theme_minimal() + 
  coord_flip() +
  xlab(element_blank()) +
  theme(axis.text.y = element_blank()) +
  ylab(element_blank()) +
  ggtitle(glue('Proportion biovolume'))

my_legend <- get_legend(aa + theme(legend.position = 'bottom'))

pp <- plot_grid(aa + theme(legend.position = 'none'), 
          bb + theme(legend.position = 'none'),
          rel_widths = c(1, 0.75))

plot_grid(pp, my_legend, ncol = 1, rel_heights = c(1, 0.2))
