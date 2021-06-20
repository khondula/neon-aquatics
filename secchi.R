# secchi depth

# DP1.20252.001
product_dir <- '~/Box/data/NEON/NEON_depth-secchi'

# has a variable for "clear to bottom" 
# Designation for when the secchi disk can be seen all the way to the bottom
# also maximum depth

library(tidyverse)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(vroom)
lake_river_sites <- c('BARC', 'SUGG', 'CRAM', 'LIRO', 'PRPO', 'PRLA', 'TOOK', 'FLNT', 'BLWA', 'TOMB', 'BLUE')

mysite <- 'BARC'
myglob <- 'dep_secchi'

update_secchi_files <- function(mysite, myglob){
  # current files
  product_dir <- '~/Box/data/NEON/NEON_depth-secchi'
  fs::dir_create(glue('{product_dir}/{mysite}'))
  current_files <- fs::dir_ls(glue('{product_dir}/{mysite}'), glob = glue("*{myglob}*"))
  filenames_split <- basename(current_files) %>% str_split("\\.", simplify = TRUE)
  months_have <- c()
  if(nrow(filenames_split > 0)){
    months_have <- filenames_split[,8]
  }
  
  base_url <- 'http://data.neonscience.org/api/v0/'
  data_id <- 'DP1.20252.001' # secchi depth
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
    dplyr::filter(!month %in% months_have) 
  
  my_site_urls <- my_site_to_get %>% pull(url)
  
  # filter to just the myglob basic files
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
    my_files_local <- glue('{product_dir}/{mysite}/{my_files$files}')
    purrr::walk2(.x = my_files$urls, .y = my_files_local, ~download.file(.x, .y))
  }
  my_files_list %>% purrr::walk(~download_month(.x))
  if(any_new){message(glue('new {myglob} data from {mysite} for: {glue_collapse(months_newfiles, ", ")}'))}
  
}

update_secchi_files('BARC', 'dep_secchi')

lake_river_sites %>% purrr::walk(~update_secchi_files(.x, myglob = 'dep_secchi'))

## Plotting

mysite <- 'PRPO'

product_dir <- '~/Box/data/NEON/NEON_depth-secchi'

# combine_secchi <- function(mysite){
#   site_secchi_df <- fs::dir_ls(glue('{product_dir}/{mysite}')) %>%
#     vroom::vroom(delim = ',')
#   secchi_path <- glue('results/secchi-x-site/{mysite}-secchi.csv')
#   site_secchi_df %>% write_csv(secchi_path)
# }
# 
# lake_river_sites %>% purrr::walk(~combine_secchi(.x))
read_csv('results/secchi-x-site/BLWA-secchi.csv') %>% head()
secchi_df <- fs::dir_ls('results/secchi-x-site') %>%
  purrr::map_dfr(~read_csv(.x)) %>%
  dplyr::select(domainID, siteID, namedLocation, date, clearToBottom, 
                maxDepth, euphoticDepth, icePresent, secchiMeanDepth, remarks)

secchi_df$namedLocation %>% unique()
secchi_df %>% dplyr::filter(siteID == 'BLWA') %>% View()

secchi_df %>%
  ggplot(aes(x = date, y = clearToBottom)) +
  geom_point(aes(fill = clearToBottom), pch = 21) +
  facet_wrap(vars(namedLocation))

aop_dates <- read_csv('../spectra/results/spectra-ids.csv') %>%
  # dplyr::filter(siteID == my_siteid) %>%
  mutate(flightdate = str_sub(flightline, 1, 8)) %>%
  mutate(flightdate = as_date(flightdate)) %>%
  dplyr::select(siteID, nmdLctn, flightdate, clouds, loctype) %>%
  rename(namedLocation = nmdLctn) %>%
  dplyr::filter(namedLocation %in% secchi_df$namedLocation)

secchi_df %>%
  ggplot(aes(x = as_date(date), y = secchiMeanDepth)) +
  geom_vline(data = aop_dates, aes(xintercept = flightdate), col = 'purple') +
  geom_line(col = 'red') +
  geom_line(aes(y = euphoticDepth), col = 'red', lwd = 0.25) +
  geom_line(aes(y = maxDepth)) +
  geom_point(aes(y = maxDepth, fill = clearToBottom, shape = icePresent), alpha = 0.75) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = list('N' = 'green', 'Y' = 'blue', 'NA' = 'gray')) +
  geom_hline(aes(yintercept = 0), col = 'blue') +
  scale_y_reverse() +
  facet_wrap(vars(namedLocation), scales = 'free') +
  theme_bw() +
  xlab(element_blank()) +
  ylab("Depth (m)") +
  ggtitle('Secchi depth (red) and max depth (black/points)')

ggsave('figs/secchi-depth.png', width = 16, height = 6)
ggsave('../spectra/figs/secchi-depth.png', width = 16, height = 6)
