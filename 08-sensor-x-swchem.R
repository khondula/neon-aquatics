library(tidyverse)
library(fs)
library(glue)
library(lubridate)
library(vroom)
library(hms)
library(data.table)

wq_dir <- '~/Box/data/NEON/NEON_water-quality'
ts_dir <- '~/Box/data/NEON/wq-timeseries'
# mysite <- 'ARIK'

# sensor positions with fDOM
# fdom_positions <- fs::dir_ls(ts_dir, recurse = 1, regexp = 'fDOM') %>%
#   basename() %>% as.data.frame() %>%
#   mutate(siteid = substr(., 1, 4),
#          sensor_position = substr(., 17, 19)) %>%
#   dplyr::select(siteid, sensor_position)
# fdom_positions %>% write_csv('results/fdom_positions.csv')

# there may be multiple sensor positions per SITE
fdom_positions <- read_csv('results/fdom_positions.csv', col_types = 'cc')

mysite <- 'HOPB'
join_doc_fdom <- function(mysite){
  # read in PREFERRED sensor positions file
  fdom_positions_aos <- read_csv('results/fdom_positions_aos.csv',
                                 col_types = 'ccc') %>%
    dplyr::filter(siteid == mysite)
  # read in all DOC data for site
  # no NAs but all sample conditions
  doc_coltypes <- 'ccccTcdcc'
  doc_df <- read_csv('results/doc_all.csv', col_types = doc_coltypes) %>%
    dplyr::filter(siteID == mysite) %>%
    dplyr::filter(!is.na(analyteConcentration)) %>%
    mutate(date = lubridate::as_date(collectDate)) %>%
    mutate(time_hms = as_hms(collectDate))
    
  # how many sites?
  doc_df$namedLocation %>% unique()
  n_samples <- doc_df %>% nrow()
  sample_dates <- doc_df$date %>% unique()
  message(glue('{n_samples} DOC samples from {length(sample_dates)} dates at {mysite}'))
  
  # read in sensor data for fDOM for site
  fdom_df_all <- glue('{ts_dir}/{mysite}') %>% 
    fs::dir_ls(glob = '*fDOM*') %>%
    vroom::vroom() %>% 
    dplyr::filter(!is.na(fDOM)) %>%
    mutate(date = lubridate::as_date(startDateTime)) %>%
    mutate(time_hms = as_hms(startDateTime))
  
  fdom_dates <- fdom_df_all$date %>% unique()
  sensor_positions <- fdom_df_all$sensor_position %>% unique()
  n_positions <- sensor_positions %>% length()
  message(glue('fDOM at {n_positions} sensor positions at {mysite}: {sensor_positions}'))
  # how much data is flagged?
  # table(fdom_df_all$fDOMFinalQF)
  # redo this with custom colors, lines
  # fdom_df_all %>% 
  #   mutate(fDOMFinalQF = as_factor(fDOMFinalQF)) %>%
  #   ggplot(aes(x = fDOM)) +
  #   geom_density(aes(color = fDOMFinalQF)) +
  #   theme_minimal() +
  #   theme(legend.position = 'bottom')
  
  # filter out all flagged data
  fdom_df_qa <- fdom_df_all %>% dplyr::filter(fDOMFinalQF %in% 0)
  fdom_qa_dates <- fdom_df_qa$date %>% unique()
  # adjust to local time for site? 
  
  dates_both <- sample_dates %>% intersect(fdom_dates) %>% as_date()
  dates_both_qa <- sample_dates %>% intersect(fdom_qa_dates) %>% as_date()
  doc_df_sub <- doc_df %>% dplyr::filter(date %in% dates_both)
  
  # subset FDOM data to sampling dates
  fdom_df_qa_sub <- fdom_df_qa %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    dplyr::filter(date %in% dates_both)
  
  fdom_df_all_sub <- fdom_df_all %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    dplyr::filter(date %in% dates_both)
    
  # if there is one sensor position
  p1 <- fdom_df_all_sub %>%
    mutate(date = as.character(date)) %>%
    ggplot(aes(x = time_hms, y = fDOM)) +
    geom_vline(data = doc_df_sub, aes(xintercept = time_hms), lty = 2) +
    geom_line(col = 'gray') +
    geom_line(data = fdom_df_qa_sub, aes(col = sensor_position)) +
    theme_minimal() +
    expand_limits(y = 0) +
    xlab("UTC time") +
    ggtitle(glue('{mysite} - fDOM on DOC grab sample days')) +
    facet_wrap(vars(date)) +
    theme(legend.position = 'right')
  p1
  
  ggsave(glue('figs/{mysite}-fdom-qa.png'), p1, width = 10, height = 6)
  ggsave(glue('figs/fdom/{mysite}-fdom.png'), p1, width = 6, height = 4)
  # facet plot with vertical lines for time of grab sample collection
  # and once time of AOP overpass is determined, facet plot with vertical lines 
  
  # NOT CONVERTED TO LOCAL TIME, but for now... 
  # average (median) of day
  # might want to know how much of the day is represented by each value
  # and how variable that day is...
  fdom_daily_df <- fdom_df_qa_sub %>%
    group_by(siteid, sensor_position, date) %>%
    summarise(median_fDOM = median(fDOM),
              sd_fDOM = sd(fDOM),
              se_fDOM = sd_fDOM/n())
  
  # JOIN!! #
  doc_x_fdom <- doc_df_sub %>% 
    left_join(fdom_positions_aos,
    by = c("siteID" = "siteid", "namedLocation")) %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    left_join(fdom_daily_df, 
              by = c("siteID" = "siteid", "date", "sensor_position"))
  
  # doc_x_fdom %>% write_csv(glue('results/doc-join-fdom/{mysite}-doc-join-fdom.csv'))
  
  # add regression info to plot
  p2 <- doc_x_fdom %>% 
    filter(!is.na(median_fDOM)) %>%
    filter(!is.na(analyteConcentration)) %>%
    ggplot(aes(x = analyteConcentration, y = median_fDOM)) +
    geom_smooth(method = 'lm', se = TRUE) +
    geom_errorbar(aes(ymin = median_fDOM - sd_fDOM, 
                      ymax = median_fDOM + sd_fDOM)) +
    geom_point(aes(fill = sampleCondition), pch = 21) +
    xlab("DOC (mg/L)") +
    # ylab("fDOM")
    theme_minimal() +
    # facet_wrap(vars(sampleCondition), scales = 'free') +
    expand_limits(x = 0, y = 0) +
    # theme(legend.position = 'none') +
    ggtitle(glue('{mysite} - fDOM vs DOC'))
  p2
  ggsave(glue('figs/fdom-x-doc/{mysite}-fdom-x-doc.png'), p2, width = 4, height = 2)
}


# do this for all the aquatic sites... with data downloaded
sites_w_ts <- fs::dir_ls(wq_dir) %>% basename()
doc_joined_sites <- fs::dir_ls('results/doc-join-fdom') %>% basename() %>% str_sub(1, 4)
poss_join_doc_fdom <- purrr::safely(join_doc_fdom, quiet = FALSE)
sites_to_join <- sites_w_ts[!sites_w_ts %in% doc_joined_sites]

sites_to_join %>% purrr::walk(~poss_join_doc_fdom(.x))

poss_join_doc_fdom("ARIK")
# and then also for TSS vs turbidity
# and for chl vs chl a samples? 
# then see how good the models are and how much they vary by site

## all doc fDOM plots
join_dir <- 'results/doc-join-fdom'
doc_x_fdom <- fs::dir_ls(join_dir) %>%
  purrr::map_dfr(~read_csv(.x))

doc_x_fdom %>% 
  filter(!is.na(median_fDOM)) %>%
  ggplot(aes(x = analyteConcentration, y = median_fDOM, color = siteID)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_errorbar(aes(ymin = median_fDOM - sd_fDOM, 
                    ymax = median_fDOM + sd_fDOM)) +
  geom_point(pch = 21, aes(fill = siteID), col = 'black') +
  xlab("DOC (mg/L)") +
  # ylab("fDOM")
  theme_minimal() +
  expand_limits(x = 0, y = 0) +
  facet_wrap(vars(siteID), scales = 'free') +
  theme(legend.position = 'none') +
  ggtitle(glue('fDOM vs DOC'))
ggsave(glue('figs/fdom-x-doc.png'), width = 8, height = 6)

# NOT faceted
doc_x_fdom %>% 
  filter(!is.na(median_fDOM)) %>%
  ggplot(aes(x = analyteConcentration, y = median_fDOM, color = siteID)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_errorbar(aes(ymin = median_fDOM - sd_fDOM, 
                    ymax = median_fDOM + sd_fDOM)) +
  geom_point(pch = 21, aes(fill = siteID), col = 'black', alpha = 0.5) +
  xlab("DOC (mg/L)") +
  # ylab("fDOM")
  theme_minimal() +
  expand_limits(x = 0, y = 0) +
  theme(legend.position = 'none') +
  ggtitle(glue('fDOM vs DOC'))

# now with doc as Y
# doc_x_fdom %>% 
#   filter(!is.na(median_fDOM)) %>%
#   ggplot(aes(x = median_fDOM, y = analyteConcentration, color = siteID)) +
#   geom_smooth(method = 'lm', se = FALSE) +
#   geom_errorbarh(aes(xmin = median_fDOM - sd_fDOM, 
#                     xmax = median_fDOM + sd_fDOM)) +
#   geom_point(pch = 21, aes(fill = siteID), col = 'black') +
#   ylab("DOC (mg/L)") +
#   # ylab("fDOM")
#   theme_minimal() +
#   expand_limits(x = 0, y = 0) +
#   facet_wrap(vars(siteID), scales = 'free') +
#   theme(legend.position = 'none') +
#   ggtitle(glue('fDOM vs DOC'))
# 
# ggsave(glue('figs/fdom-x-doc2.png'))

# TURBIDITY

fs::dir_ls(ts_dir, recurse = 1, regexp = 'turbidity') %>% 
  basename() %>% as.data.frame() %>%
  mutate(siteid = substr(., 1, 4),
         sensor_position = substr(., 22, 24)) %>%
  dplyr::select(siteid, sensor_position)
