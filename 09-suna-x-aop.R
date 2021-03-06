library(tidyverse)
library(fs)
library(glue)
library(lubridate)
library(vroom)
library(hms)
library(data.table)

suna_dir <- '~/Box/data/NEON/NEON_nitrate'
suna_ts_dir <- '~/Box/data/NEON/suna-timeseries'

aop_dates <- read_csv('results/sites_join_aop_dates.csv')
get_aop_dates <- function(aq_siteids){
  aop_dates <- read_csv('results/sites_join_aop_dates.csv') %>%
    dplyr::filter(siteID %in% aq_siteids) %>%
    dplyr::select(siteID, aop_site_id, flightdate) %>%
    arrange(flightdate) %>% distinct()
  return(aop_dates)
}

mysite <- 'CARI'

# siteID is the aquatic site

aop_dates <- get_aop_dates(mysite) %>% pull(flightdate)
aop_years <- as.Date(aop_dates) %>% lubridate::year() %>% unique()

message(glue('{length(aop_dates)} flight dates at {mysite} over {glue_collapse(aop_years, ", ")}'))
  
# read in sensor data for site
suna_df_all <- glue('{suna_ts_dir}/{mysite}') %>% 
    fs::dir_ls(glob = '*SUNA*') %>%
    vroom::vroom() %>% 
    dplyr::filter(!is.na(surfWaterNitrateMean)) %>%
    mutate(date = lubridate::as_date(startDateTime)) %>%
  mutate(year = lubridate::year(startDateTime)) %>%
    mutate(time_hms = as_hms(startDateTime))
  
suna_dates <- suna_df_all$date %>% unique()
sensor_positions <- suna_df_all$sensor_position %>% unique()
n_positions <- sensor_positions %>% length()
message(glue('SUNA at {n_positions} sensor positions at {mysite}: {sensor_positions}'))

aop_dates_df <- data.frame(startDateTime = lubridate::as_datetime(aop_dates)) %>%
  mutate(year = lubridate::year(startDateTime))

# filter out all flagged data
suna_df_qa <- suna_df_all %>% dplyr::filter(finalQF %in% 0)
suna_qa_dates <- suna_df_qa$date %>% unique()

suna_df_all %>%
  ggplot(aes(x = startDateTime, y = surfWaterNitrateMean)) +
  geom_vline(data = aop_dates_df, aes(xintercept = startDateTime),
             col = 'red', lwd = 0.5) +
  geom_point(size = 0.5, alpha = 0.5, col = 'gray') +
  geom_point(data = suna_df_qa, size = 0.5) +
  facet_wrap(vars(sensor_position), ncol =1) +
  theme_minimal() +
  ggtitle(glue('SUNA and flights at {mysite}'))

ggsave(glue('figs/suna-x-aop/{mysite}_suna-x-aop-ts.png'), width = 10, height = 6)

dates_both <- aop_dates %>% intersect(suna_dates) %>% as_date()
dates_both_qa <- aop_dates %>% intersect(suna_qa_dates) %>% as_date()
  
# subset suna data to sampling dates
suna_df_qa_sub <- suna_df_qa %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    dplyr::filter(date %in% dates_both)
  
suna_df_all_sub <- suna_df_all %>%
    mutate(sensor_position = as.character(sensor_position)) %>%
    dplyr::filter(date %in% dates_both)
    
# if there is one sensor position
p1 <- suna_df_all_sub %>%
    mutate(date = as.character(date)) %>%
    ggplot(aes(x = time_hms, y = surfWaterNitrateMean)) +
    geom_line(col = 'gray', aes(group = sensor_position)) +
    geom_line(data = suna_df_qa_sub, aes(col = sensor_position)) +
    theme_minimal() +
    expand_limits(y = 0) +
    xlab("UTC time") +
    ggtitle(glue('{mysite} - SUNA on AOP flight days')) +
    facet_wrap(vars(date)) +
    theme(legend.position = 'right')
p1
  
ggsave(glue('figs/suna-x-aop/{mysite}_suna-x-aop.png'), p1, width = 10, height = 6)


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


## with aos dates now

# suna_x_aop <- read_csv('~/Box/data/NEON/meta/suna-aop-summary_csv.csv')
suna_x_aop <- readxl::read_excel('~/Box/data/NEON/meta/suna-aop-summary.xlsx', 
                                 col_types = c("text", "text", "numeric",
                                               "text", "date", "date", "text",
                                               "text", "text", "date", "date",
                                               "text", "guess", "guess", "guess")) %>%
  mutate(first_aop_date = as_date(first_aop_date),
         last_aop_date = as_date(last_aop_date),
         aos_pre = as_date(aos_pre),
         aos_post = as_date(aos_post))

suna_x_aop_df <- suna_x_aop %>%
  mutate(range_start = aos_pre - dweeks(1),
         range_end = aos_post + dweeks(1))

suna_x_aop_df %>% write_csv('results/get-raw-suna.csv')

suna_x_aop_df$get_raw_suna %>% table()
