# calculating Spectral Slope
# from SUVA 254 and 280
# need DOC measurements to 
# convert SUVA to absorbance 

head(suva254_df)
head(suva280_df)

suva254_df <- suva254_df %>%
  rename(suva254 = analyteConcentration) %>%
  dplyr::select(-analyte, -analyteUnits, -uid)
suva280_df <- suva280_df %>%
  rename(suva280 = analyteConcentration) %>%
  dplyr::select(-analyte, -analyteUnits, -uid)

suva_join <- suva254_df %>% 
  left_join(suva280_df, by = c('domainID', 'siteID', 'namedLocation', 
                               'collectDate', 'sampleCondition')) %>%
  dplyr::select(domainID, siteID, namedLocation, collectDate, 
                sampleCondition, suva254, suva280) %>%
  mutate(date = lubridate::as_date(collectDate),
         bothsuva = !is.na(suva254) & !is.na(suva280))

suva_join$bothsuva %>% table()
head(suva_join)

# check for sample condition

suva_join %>%
  tidyr::pivot_longer(cols = c(suva254, suva280), 
                      names_to = 'wavelength', values_to = 'absorbance') %>%
  ggplot(aes(x = wavelength, y = absorbance, fill = namedLocation)) +
  geom_boxplot(aes(fill = namedLocation)) +
  facet_wrap(vars(siteID), scales = 'free_y') +
  theme_bw() +
  theme(legend.position = 'none')
