
library(ggplot2); library(statar)
source("D:/OneDrive - hawaii.edu/Documents/Projects/Packages/Demean/demeanByGroup.R")

# load data
dat <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HonoluluBWS/Processed/hbws_dat.rds")

# check range of starting and ending dates
dates <- data.frame(date = c(dat$BILLING_START, dat$BILLING_END),
                    type = rep(c('Start', 'End'), each = nrow(dat)))
dates$year_month <- substr(dates$date, 1, 7)

ggplot(dates) + geom_bar(aes(year_month, fill = type, color = type), alpha = 0.5, position = 'stack') +
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 90)) +
  labs(x = NULL, y = 'Num obs', color = NULL, fill = NULL)

rm(dates)

# limit data to remove incomplete months
dat <- dat[dat$BILLING_START >= as.POSIXct('2013-02-01') & dat$BILLING_END <= as.POSIXct('2020-10-31'),]
dat$BILLING_CONSUMPTION <- dat$BILLING_CONSUMPTION * 1000

# examine aggregate water use
dat$year_month <- as.POSIXct(paste0(substr(dat$BILLING_START, 1, 7), '-01'))
water_use_agg <- aggregate(dat$BILLING_CONSUMPTION, list(dat$year_month), sum)
colnames(water_use_agg) <- c('Date', 'WaterUse')
ggplot(water_use_agg[-nrow(water_use_agg),], aes(x = Date, y = WaterUse/1e9)) + geom_line() +
  geom_vline(xintercept = as.POSIXct('2020-03-01'), color = 'red', size = 1, alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  labs(x = NULL, y = 'Aggregate water use (bil gal per month)')

# examine water use by sector
water_use_agg <- aggregate(dat$BILLING_CONSUMPTION, list(dat$PREMISE_TYPE, dat$year_month), sum)
colnames(water_use_agg) <- c('Type', 'Date', 'WaterUse')
ggplot(water_use_agg[water_use_agg$Date <= as.POSIXct('2020-09-30'),], aes(x = Date, y = WaterUse/1e9, color = Type)) + geom_line() +
  geom_vline(xintercept = as.POSIXct('2020-03-01'), color = 'red', size = 2, alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  labs(x = NULL, y = 'Aggregate water use (bil gal per month)')

water_use_agg_split <- split(water_use_agg, water_use_agg$Type)
water_use_agg_split <- lapply(water_use_agg_split, function(df){df$WaterUse <- df$WaterUse/(df$WaterUse[[1]]); df})
water_use_agg       <- do.call(rbind, water_use_agg_split)
ggplot(water_use_agg[water_use_agg$Date <= as.POSIXct('2020-09-30') & water_use_agg$Type %in% c('SFD', 'LO-RISE', 'HI-RISE', 'COM', 'HOTEL'),], aes(x = Date, y = WaterUse, color = Type)) + geom_line() +
  geom_vline(xintercept = as.POSIXct('2020-03-01'), color = 'red', size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'longdash', size = 1) +
  theme(text = element_text(size = 15)) +
  labs(x = NULL, y = 'Water use relative to 2/2013')
