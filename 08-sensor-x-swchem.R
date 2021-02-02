library(tidyverse)
library(fs)
library(glue)
library(lubridate)
library(vroom)
library(hms)

# once surface water chem data and sensor data are acquired
# compare sensor values and surface water chem
# overall distribution AND for days of sw sampling

ts_dir <- '~/Box/data/NEON/wq-timeseries'

# first for DOC at COMO
siteid <- 'COMO'

doc_df <- read_csv('results/doc_all.csv') %>%
  dplyr::filter(siteID == siteid) %>%
  mutate(date = lubridate::as_date(collectDate))

# how many sites?
doc_df$namedLocation %>% unique()
sample_dates <- doc_df$date %>% unique()

# read in sensor data for fDOM for site
fdom_df_all <- glue('{ts_dir}/{siteid}') %>% 
  fs::dir_ls(glob = '*fDOM_sensor102*') %>%
  vroom() %>% 
  dplyr::filter(!is.na(fDOM)) %>%
  mutate(date = as_date(startDateTime))

# why are there still NAs here? maybe read in as NULLs in 07 fxn?
# head(fdom_df, 1000) %>% View()
# how much data is flagged?
table(fdom_df$fDOMFinalQF)

# redo this with custom colors, lines
fdom_df_all %>% 
  mutate(fDOMFinalQF = as_factor(fDOMFinalQF)) %>%
  ggplot(aes(x = fDOM)) +
  geom_density(aes(color = fDOMFinalQF)) +
  theme_minimal() +
  theme(legend.position = 'bottom')

fdom_df <- fdom_df_all %>% 
  dplyr::filter(fDOMFinalQF %in% 0)

# comparing distributions could make a nice ggridges plot?
fdom_df %>%
  ggplot(aes(x = fDOM)) +
  geom_density() + 
  theme_minimal()

# get values on sampling dates, plot on density plot to see range sampled
# adjust to local time for site? 
fdom_df %>%
  dplyr::filter(date %in% sample_dates) %>%
  mutate(time_hms = as_hms(startDateTime)) %>% 
  ggplot(aes(x = time_hms, y = fDOM, group = date)) +
  geom_line(aes(col = date)) + 
  theme_minimal() +
  expand_limits(y = 0) +
  xlab("UTC time") +
  ggtitle(glue('{siteid} fDOM on grab sample days'))
# figure out a better color palette, facet by year and color by date?

# once time of AOP overpass is determined, facet plot with vertical lines 