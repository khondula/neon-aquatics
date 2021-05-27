# compare above and below water PAR

mysite <- 'BARC'
par_above_dir <- '~/Box/data/NEON/NEON_par-water-surface'
par_below_dir <- '~/Box/data/NEON/NEON_par-below-water-surface'
my_positions <- '103'
# uPAR_30min
# PARWS_30min

# 103: sensors mounted on buoys in lakes or rivers
# 130 and 140: sensors mounted in the littoral zone of lakes

rearrange_sensor_data <- function(mysite, 
                                  par_above_dir,
                                  par_below_dir){
  # read all files in together
  parA_coltypes <- "TTdddddddd"
  parA_df <- glue('{par_above_dir}/{mysite}') %>%
    fs::dir_ls(glob = "*PARWS_30min*") %>%
    purrr::map_dfr(~read_csv(.x, col_types = parA_coltypes), .id = 'filename') %>%
    dplyr::mutate(filename = basename(filename)) %>% 
    dplyr::mutate(siteid = substr(filename, 10, 13),
                  sensor_position = substr(filename, 29, 31)) %>%
    dplyr::select(-filename)
  
  parB_coltypes <- glue("TT{glue_collapse(rep('d', 16))}")
  parB_df <- glue('{par_below_dir}/{mysite}') %>%
    fs::dir_ls(glob = "*uPAR_30min*") %>%
    purrr::map_dfr(~read_csv(.x, col_types = parB_coltypes), .id = 'filename') %>%
    dplyr::mutate(filename = basename(filename)) %>% 
    dplyr::mutate(siteid = substr(filename, 10, 13),
                  sensor_position = substr(filename, 29, 31)) %>%
    dplyr::select(startDateTime:uPARFinalQF, siteid, sensor_position) %>%
    dplyr::filter(sensor_position %in% my_positions)
  
  par_join_df <- parA_df %>% 
    left_join(parB_df, by = c('startDateTime', 'endDateTime', 'siteid', 'sensor_position'))
  
  
  #  then save separate CSV for each parameter and sensor
  # need to have wq_df, wq_params, ts_dir, siteid
  parts_dir <- '~/Box/data/NEON/par-timeseries'
  fs::dir_create(parts_dir)
  # save_par_timeseries <- function(wq_param){
  #   # list of parameter data for each sensor
  #   wq_par_list <- wq_df %>% 
  #     # filter(!is.na(wq_params[[wq_param]][1])) %>%
  #     dplyr::select(siteid, sensor_position, startDateTime, 
  #                   endDateTime, wq_params[[wq_param]]) %>%
  #     group_by(sensor_position) %>% group_split()
  #   # name list with each sensor
  #   sensorids <- wq_par_list %>% 
  #     purrr::map_chr(~unique(pull(.x, sensor_position)))
  #   names(wq_par_list) <- sensorids
  #   ## HERE -- check for any non NA values before saving? ##
  #   # make directory and save files
  #   site_ts_dir <- glue('{ts_dir}/{mysite}')
  #   fs::dir_create(site_ts_dir)
  #   filenames <- glue('{site_ts_dir}/{mysite}_{wq_param}_sensor{sensorids}.csv')
  #   wq_par_list %>% 
  #     purrr::walk2(.y = filenames, ~vroom_write(.x, .y, delim = ","))
  # }
  
  # names(wq_params) %>% purrr::walk(~save_sensor_wq_timeseries(.x))
}

parA_df %>%
  dplyr::filter(PARMean > 0) %>% 
  dplyr::filter(PARFinalQF == 0) %>%
  mutate(year = lubridate::year(startDateTime)) %>%
  ggplot(aes(x = startDateTime, y = PARMean)) + 
  geom_line() +
  facet_wrap(vars(year), scales = 'free')

par_join_df %>%
  dplyr::filter(PARFinalQF == 0) %>%
  dplyr::filter(uPARFinalQF == 0) %>%
  mutate(date = lubridate::as_date(startDateTime)) %>%
  dplyr::filter(date > '2020-04-01' & date < '2020-04-15') %>% 
  mutate(timeofday = hms::as_hms(startDateTime)) %>%
  ggplot(aes(x = startDateTime, y = PARMean)) + 
  geom_line(col = 'blue') +
  geom_line(aes(y = uPARMean), col = 'red')

par_join_df %>%
  dplyr::filter(uPARMean > 0) %>%
  dplyr::filter(PARFinalQF == 0) %>%
  dplyr::filter(uPARFinalQF == 0) %>%
  mutate(date = lubridate::as_date(startDateTime)) %>%
  dplyr::filter(date > '2020-05-01' & date < '2020-05-15') %>% 
  mutate(timeofday = hms::as_hms(startDateTime)) %>%
  ggplot(aes(x = startDateTime, y = uPARMean/PARMean)) + 
  geom_hline(yintercept = 0) +
  geom_point(pch = 21, alpha = 0.5, fill = 'red') +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw()
