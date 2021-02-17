library(tidyverse)
library(fs)
library(glue)
library(lubridate)
library(vroom)
library(hms)
library(data.table)

fdom_dir <- '~/Box/data/NEON/NEON_water-quality/'
wq_ts_dir <- '~/Box/data/NEON/wq-timeseries/'
qf_cols_list <- list(fDOM = 'fDOMFinalQF')

aop_dates <- read_csv('results/sites_join_aop_dates.csv')
get_aop_dates <- function(aq_siteids){
  aop_dates <- read_csv('results/sites_join_aop_dates.csv') %>%
    dplyr::filter(siteID %in% aq_siteids) %>%
    dplyr::select(siteID, aop_site_id, flightdate) %>%
    arrange(flightdate) %>% distinct()
  return(aop_dates)
}

aq_site_ids <- read_lines('aq_site_ids.txt')
mysite <- aq_site_ids[1]
myparam <- 'fDOM'
myQFcol <- qf_cols_list[[myparam]]
# siteID is the aquatic site

aop_dates <- get_aop_dates(mysite) %>% pull(flightdate)
aop_years <- as.Date(aop_dates) %>% lubridate::year() %>% unique()

message(glue('{length(aop_dates)} flight dates at {mysite} over {glue_collapse(aop_years, ", ")}'))
  
# read in sensor data for site
wq_df_all <- glue('{wq_ts_dir}/{mysite}') %>% 
  fs::dir_ls(glob = glue('*{myparam}*')) %>%
  vroom::vroom() %>% 
  dplyr::filter(!is.na(fDOM)) %>%
  mutate(date = lubridate::as_date(startDateTime)) %>%
  mutate(year = lubridate::year(startDateTime)) %>%
  mutate(time_hms = as_hms(startDateTime))
  
wq_dates <- wq_df_all$date %>% unique()
sensor_positions <- wq_df_all$sensor_position %>% unique()
n_positions <- sensor_positions %>% length()
message(glue('{myparam} at {n_positions} sensor positions at {mysite}: 
             {glue_collapse(sensor_positions, ", ")}'))

aop_dates_df <- data.frame(startDateTime = lubridate::as_datetime(aop_dates)) %>%
  mutate(year = lubridate::year(startDateTime))

# filter out all flagged data
wq_df_qa <- wq_df_all %>% 
  dplyr::filter(!!sym(myQFcol) %in% 0)
wq_qa_dates <- wq_df_qa$date %>% unique()

wq_df_all %>% 
  ggplot(aes(x = startDateTime, y = !!sym(myparam))) +
  geom_vline(data = aop_dates_df, aes(xintercept = startDateTime),
             col = 'red', lwd = 0.5) +
  geom_point(size = 0.5, alpha = 0.5, col = 'gray') +
  geom_point(data = wq_df_qa, size = 0.5) +
  facet_wrap(vars(sensor_position), ncol =1) +
  theme_minimal() +
  ggtitle(glue('{myparam} and flights at {mysite}'))

ggsave(glue('figs/{myparam}-x-aop/{mysite}_{myparam}-x-aop-ts.png'), width = 10, height = 6)

dates_both <- aop_dates %>% intersect(wq_dates) %>% as_date()
dates_both_qa <- aop_dates %>% intersect(wq_qa_dates) %>% as_date()
  
# subset WQ data to sampling dates
wq_df_qa_sub <- wq_df_qa %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    dplyr::filter(date %in% dates_both)
  
wq_df_all_sub <- wq_df_all %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    dplyr::filter(date %in% dates_both)
    
# if there is one sensor position
p1 <- wq_df_all_sub %>%
    mutate(date = as.character(date)) %>%
    ggplot(aes(x = time_hms, y = !!sym(myparam))) +
    geom_line(col = 'gray', aes(group = sensor_position)) +
    geom_line(data = wq_df_all_sub, aes(col = sensor_position)) +
    theme_minimal() +
    expand_limits(y = 0) +
    xlab("UTC time") +
    ggtitle(glue('{mysite} - {myparam} on AOP flight days')) +
    facet_wrap(vars(date)) +
    theme(legend.position = 'right')
p1
  
ggsave(glue('figs/{myparam}-x-aop/{mysite}_{myparam}-x-aop.png'), p1, width = 10, height = 6)


suna_x_aop_df2 <- readxl::read_excel('~/Box/data/NEON/meta/SUNA-x-AOP.xlsx',
                                     col_types = c("text", "text", "text",
                                                   "text", "text", "text",
                                                   "numeric", "date", "text",
                                                   "logical", "logical", "guess", "guess"))
suna_x_aop_df2 <- suna_x_aop_df2 %>% 
  rename(SUNA = 10) %>%
  dplyr::filter(SUNA)

suna_x_aop_summary <- suna_x_aop_df2 %>%
  group_by(domanID, siteid, year, sensor_position) %>%
  mutate(flightdate = lubridate::as_date(flightdate)) %>%
  summarise(first_aop_date = min(flightdate),
            last_aop_date = max(flightdate)) 

suna_x_aop_summary %>% write_csv('results/suna-aop-summary.csv')
