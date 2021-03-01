
library(dplyr)

# load data
dat_bws <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HonoluluBWS/Processed/hbws_dat_aligned_billing_periods.rds")
dat_sg  <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/03-SafeGraph-with-tmk.rds")

# keep mutual TMKs
dat_bws$tmk <- as.character(gsub("[^0-9\\.]", "", dat_bws$TMK)); dat_bws$TMK <- NULL
colnames(dat_sg)[colnames(dat_sg)   == 'TMK9NUM'] <- 'tmk'
dat_bws <- dat_bws[dat_bws$tmk %in% dat_sg$tmk,]
dat_sg  <- dat_sg[dat_sg$tmk %in% dat_bws$tmk,]

# rename bws columns
colnames(dat_bws) <- c('bws_acct_num', 'premise_type', 'date', 'gal_total_monthly', 'gal_avg_daily', 'tmk')




#### merge data ----

# create month midpoint variable for SafeGraph data
dat_sg$date <- as.Date(paste0(substr(as.character(dat_sg$date_range_start), 1, 7), '-15'))

# merge data
dat <- left_join(dat_bws, dat_sg, c('tmk', 'date'))

# remove rows with missing data
dat <- dat[!is.na(dat$safegraph_place_id),]




#### aggregate data ----

# some TMKs have more than one location associated with them, so aggregate foot traffic
agg_dat <- aggregate(list(dat$gal_total_monthly, dat$gal_avg_daily, dat$raw_visit_counts, dat$raw_visitor_counts, dat$median_dwell), list(dat$tmk, dat$date), sum)
colnames(agg_dat) <- c('tmk', 'date', 'gal_total_monthly', 'gal_avg_daily', 'raw_visit_counts', 'raw_visitor_counts', 'median_dwell_sumLocations')

# dwell times: get median as well in case sum doesn't make sense?
dwell_median <- aggregate(dat$median_dwell, list(dat$tmk, dat$date), median)
colnames(dwell_median) <- c('tmk', 'date', 'median_dwell')

# merge aggregated median dwell
dat_agg <- left_join(agg_dat, dwell_median, c('tmk', 'date'))
rm(agg_dat, dwell_median)

# merge TMK type and parent/child status
  #tmk_categories <- split(dat, dat$tmk)
  #tmk_categories <- lapply(tmk_categories, function(df){df[c('tmk', 'top_category')]})
tmk_dat <- dat[!duplicated(dat$tmk), c('tmk', 'premise_type', 'parent_safegraph_place_id')]
dat_agg <- left_join(dat_agg, tmk_dat, 'tmk')
rm(tmk_dat)



# save data
saveRDS(dat_agg, file = 'D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Processed/01_merged_BWS_and_SafeGraph_data.rds')




#### analysis ----

# water use per visitor at different levels of visitors
mean_water_use <- aggregate(dat_agg$gal_avg_daily, list(dat_agg$tmk), mean, na.rm = TRUE); colnames(mean_water_use) <- c('tmk', 'gal_avg_daily_TMKmean')
dat_agg <- dplyr::left_join(dat_agg, mean_water_use, 'tmk'); rm(mean_water_use)
dat_agg$gal_avg_daily_per_visitor_demeaned <- dat_agg$gal_avg_daily / dat_agg$gal_avg_daily_TMKmean / dat_agg$raw_visitor_counts
mean_visitors <- aggregate(dat_agg$raw_visitor_counts, list(dat_agg$tmk), mean, na.rm = TRUE); colnames(mean_visitors) <- c('tmk', 'raw_visitor_counts_TMKmean')
dat_agg <- dplyr::left_join(dat_agg, mean_visitors, 'tmk'); rm(mean_visitors)
dat_agg$raw_visitor_counts_demeaned <- dat_agg$raw_visitor_counts / dat_agg$raw_visitor_counts_TMKmean
ggplot(data = dat_agg, aes(x = raw_visitor_counts_demeaned, y = gal_avg_daily_per_visitor_demeaned)) + geom_point(alpha = 0.1, shape = 16) +
  labs(x = 'TMK-demeaned visitor count', y = 'TMK-demeaned gal/day/visitor') +
  theme(text = element_text(size = 15)) +
  scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,2))
