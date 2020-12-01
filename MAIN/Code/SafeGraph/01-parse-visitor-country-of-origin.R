

##### setup #####


# packages
library(ggplot2); library(scales); library(stringr); library(doParallel); library(plyr)
registerDoParallel(cores = 8)

# load SafeGraph data
dat <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/00-merged-location-and-foot-traffic.rds')

# define top categories
topCategories <- c('Clothing Stores', 'Museums, Historical Sites, and Similar Institutions', 'Personal Care Services', 'Religious Organizations', 'Restaurants and Other Eating Places')

# limit data to top categories and remove first two months
dat <- dat[dat$top_category %in% topCategories,]
dat <- dat[dat$date_range_start >= as.POSIXct('2018-03-01 00:00:00'),]




##### format visitor origin #####

# extract visitor country of origin
vcoo <- dat$visitor_country_of_origin

# split into a list
vcoo <- split(vcoo, seq_along(vcoo))

# split each list element by the comma
vcoo <- sapply(vcoo, strsplit, ',')

# further split by colon (:) - observations become lists, where length of list is the number of unique countries.
    # First value of each list element is country of origin, second value is number of visitors from that country
vcoo <- sapply(vcoo, strsplit, ':')

# convert to data.frames
vcoo_df <- foreach(i = 1:length(vcoo), .packages = 'stringr') %dopar% {

  # if vector is empty, return NA, otherwise format the data
  if(vcoo[[i]] == '{}' | is.na(vcoo[[i]])){ return(NA) } else {

    # get list element
    vcooi <- vcoo[[i]]

    # convert to data.frame by vectorizing the country and visitor data
    vcooi <- data.frame(country    = sapply(vcooi, function(x) x[[1]] ),
                        n_visitors = sapply(vcooi, function(x) x[[2]] ))

    # remove non-alphanumeric characters from each element, convert n_visitors to numeric
    vcooi$country <-               str_replace_all(vcooi$country,    '[^[:alnum:]]', '')
    vcooi$n_visitors <- as.numeric(str_replace_all(vcooi$n_visitors, '[^[:alnum:]]', ''))

    return(vcooi)

  }

}

# add dates
vcoo_df <- foreach(i = 1:length(vcoo_df)) %dopar% {
  vcoo_df[[i]]$date_range_start <- dat$date_range_start[[i]]
  vcoo_df[[i]]$date_range_end   <- dat$date_range_end[[i]]
  return(vcoo_df[[i]])
}

# combine into a single data.frame
vcoo_df <- lapply(vcoo_df, as.data.frame)
vcoo_df <- do.call(rbind.fill, vcoo_df)

# plot total foot traffic
vcoo_df_agg <- aggregate(vcoo_df$n_visitors, list(vcoo_df$date_range_start), sum, na.rm = TRUE)
ggplot(data = vcoo_df_agg) + geom_line(aes(x=Group.1, y=x)) + labs(x=NULL, y='Foot traffic, all visitors') +
  theme(text = element_text(size = 15))
ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateFootTraffic.png",
       dpi = 300, height = 5, width = 12)

# plot US foot traffic
vcoo_df_US <- vcoo_df[vcoo_df$country == 'US',]
vcoo_df_US_agg <- aggregate(vcoo_df_US$n_visitors, list(vcoo_df_US$date_range_start), sum, na.rm = TRUE)
ggplot(data = vcoo_df_US_agg) + geom_line(aes(x=Group.1, y=x)) + labs(x=NULL, y='Foot traffic, US visitors') +
  theme(text = element_text(size = 15))
ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateUSFootTraffic.png",
       dpi = 300, height = 5, width = 12)

# plot non-US foot traffic
vcoo_df_nonUS <- vcoo_df[vcoo_df$country != 'US',]
vcoo_df_nonUS_agg <- aggregate(vcoo_df_nonUS$n_visitors, list(vcoo_df_nonUS$date_range_start), sum, na.rm = TRUE)
ggplot(data = vcoo_df_nonUS_agg) + geom_line(aes(x=Group.1, y=x)) + labs(x=NULL, y='Foot traffic, non-US visitors') +
  theme(text = element_text(size = 15))
ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateNonUSFootTraffic.png",
       dpi = 300, height = 5, width = 12)




##### format dwell times #####

# first plot the median dwell time
dwell_time <- aggregate(dat$median_dwell, list(dat$date_range_start), median)
ggplot(data = dwell_time) + geom_line(aes(x = Group.1, y = x), size = 0.8) +
  labs(x = 'Date', y = 'Median visit length (min/visitor)') +
  scale_x_datetime(date_breaks = '4 months', labels = date_format('%Y-%m')) +
  scale_y_continuous(limits = c(20, 40)) +
  theme(text = element_text(size = 15))
ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateMedianLengthOfStay.png",
       dpi = 300, height = 5, width = 10)

rm(dwell_time)

# aggregate median dwell time by category
dwellTimeByCat <- aggregate(dat$median_dwell, list(dat$date_range_start, dat$top_category), median)
colnames(dwellTimeByCat) <- c('Date', 'Category', 'Median time')

categoryLabels <- data.frame(Category = c('Clothing Stores', 'Museums, Historical Sites, and Similar Institutions', 'Personal Care Services', 'Religious Organizations', 'Restaurants and Other Eating Places'),
                             label    = factor(c('Clothing Stores', 'Museums, Historical Sites,\nand Similar Institutions', 'Personal Care Services', 'Religious Organizations', 'Restaurants and Other\nEating Places')))
dwellTimeByCat <- dplyr::left_join(dwellTimeByCat, categoryLabels, by = 'Category')

ggplot(data = dwellTimeByCat) + geom_line(aes(x = Date, y = `Median time`, color = Category), size = 0.8) +
  labs(x = 'Date', y = 'Median visit length (min/visitor)') +
  scale_x_datetime(date_breaks = '4 months', labels = date_format('%Y-%m')) +
  scale_color_discrete(name = 'Category') +
  theme(text = element_text(size = 15),legend.key.size = unit(3, 'lines'))
ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateMedianLengthOfStayByCat.png",
       dpi = 300, height = 5, width = 12)



