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
  mutate(siteID = fct_reorder(siteID, .x = analyteConcentration, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = siteID, y = analyteConcentration)) +
  geom_boxplot() +
  theme_bw() +
  xlab(element_blank()) +
  ylab("DOC (mg/L)") +

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
