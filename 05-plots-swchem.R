# boxplots swchem by site

doc_df <- read_csv('results/doc_all.csv')

doc_df %>%
  mutate(siteID = fct_reorder(siteID, .x = analyteConcentration, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = siteID, y = analyteConcentration)) +
  geom_boxplot() +
  theme_bw() +
  xlab(element_blank()) +
  ylab("DOC (mg/L)") +
  coord_flip()

ggsave('figs/doc-x-site.png')  

# time series with flight dates
doc_df %>%
  left_join(sites_x_aop, by = c("namedLocation", "siteID", "domainID" = "domanID")) %>%
  mutate(date = as_date(collectDate)) %>%
  ggplot(aes(x = date, y = analyteConcentration, group = namedLocation)) +
  geom_line(aes(col = sitetype)) + 
  geom_point(aes(fill = sitetype), pch = 21) +
  theme_bw() +
  xlab(element_blank()) +
  ylab("DOC (mg/L)") +
  theme(legend.position = "none") +
  facet_wrap(vars(siteID), scales = "free_y")

ggsave('figs/doc-timeseries.png', height = 10, width = 18)  

tss_df <- read_csv('results/tss_all.csv')
# lots of 0s in tss data
tss_df %>%
  mutate(siteID = fct_reorder(siteID, .x = analyteConcentration, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = siteID, y = analyteConcentration)) +
  geom_boxplot() +
  theme_bw() +
  xlab(element_blank()) +
  ylab("TSS (mg/L) < 100") +
  # scale_y_log10() +
  ylim(0, 100) +
  coord_flip()

ggsave('figs/tss-x-site.png')  
