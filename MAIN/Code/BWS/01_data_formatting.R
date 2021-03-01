
library(tidyr); library(doParallel)

registerDoParallel(cores = detectCores()-1)


# load data
dat <- readr::read_tsv("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HonoluluBWS/Raw/hbws_raw.tsv.zip", skip = 1)



#### format data ----

# remove row of lines
dat <- dat[-1,]

# remove unneeded columns
dat <- dat[c(1,4,5,13,14,15,16)]

# change format of variables
dat$BILLING_START <- as.Date(dat$BILLING_START)
dat$BILLING_END   <- as.Date(dat$BILLING_END)
dat$BILLING_END <- dat$BILLING_END - 1   # subtract one day from billing period so it doesn't overlap with the next period
dat$BILLING_CONSUMPTION <- as.numeric(dat$BILLING_CONSUMPTION)
dat$BILLING_PERIOD_DAYS <- as.numeric(dat$BILLING_PERIOD_DAYS)
dat$TMK <- gsub("[^0-9]", "", dat$TMK)

# get average daily water use
dat$avg_daily_gal <- dat$BILLING_CONSUMPTION * 1000 / dat$BILLING_PERIOD_DAYS




#### align billing periods ----

# split dat by tmk
dat_split <- split(dat, dat$TMK)
dat_split <- lapply(dat_split, as.data.frame)

# get average daily and total sum of monthly water use
Sys.time()
dat_split_aligned <- lapply(dat_split, function(tmki){

  # get tmk data
  tmk_i <- tmki[c('TMK', 'BILLING_START', 'avg_daily_gal')]
  colnames(tmk_i) <- c('TMK', 'date', 'avg_daily_gal')

  # create new data.frame with daily values
  tmk_i_daily <- data.frame(date = seq.Date(min(tmk_i$date), max(tmk_i$date), by = 'day'))
  tmk_i_daily <- dplyr::left_join(tmk_i_daily, tmk_i,'date')
  tmk_i_daily <- tmk_i_daily %>% fill(avg_daily_gal)                               # fill daily values according to average daily value
  tmk_i_daily$month_year <- as.Date(paste0(substr(tmk_i_daily$date,1,7), '-15'))   # create year-month variable

  # aggregate water consumption by month (sum and mean daily)
  tmk_gal_monthly_sum       <- aggregate(tmk_i_daily$avg_daily_gal, list(tmk_i_daily$month_year), sum); colnames(tmk_gal_monthly_sum) <- c('date', 'gal_total_monthly')
  tmk_gal_monthly_meanDaily <- aggregate(tmk_i_daily$avg_daily_gal, list(tmk_i_daily$month_year), mean); colnames(tmk_gal_monthly_meanDaily) <- c('date', 'gal_avg_daily')
  tmk_water_use <- dplyr::left_join(tmk_gal_monthly_sum, tmk_gal_monthly_meanDaily, 'date')
  tmk_water_use$TMK <- unique(tmk_i$TMK)

  return(tmk_water_use)

})

dat_split_aligned <- do.call(rbind, dat_split_aligned)

# merge TMK and customer number
dat <- dat[c('ACCOUNT', 'TMK', 'PREMISE_TYPE')]
dat <- dat[!duplicated(dat$TMK),]
dat <- dplyr::left_join(dat, dat_split_aligned, 'TMK')

saveRDS(dat, file = "D:/OneDrive - hawaii.edu/Documents/Projects/Data/HonoluluBWS/Processed/hbws_dat_aligned_billing_periods.rds")
