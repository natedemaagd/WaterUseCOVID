

##### setup #####


# packages
library(ggplot2); library(scales)

# load Hawaii foot traffic data and combine
footTraffic2018 <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Monthly places patterns (foot traffic - Hawaii only)/2018/wholeYear2018.rds")
footTraffic2019 <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Monthly places patterns (foot traffic - Hawaii only)/2019/wholeYear2019.rds")
footTraffic2020 <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Monthly places patterns (foot traffic - Hawaii only)/2020/wholeYear2020.rds")
footTraffic <- do.call('rbind', list(footTraffic2018, footTraffic2019, footTraffic2020))
rm(footTraffic2018, footTraffic2019, footTraffic2020)

# load locations data
locations <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/SafeGraphHawaiiLocations.rds")

# merge locations and foot traffic data
cols_to_remove <- colnames(locations)[which(colnames(locations) %in% colnames(footTraffic))][-1]
locations <- locations[!(colnames(locations)) %in% cols_to_remove]  # except for the first column (needed to merge), remove all duplicate columns between the two data sets
footTraffic <- merge(footTraffic, locations, by = 'safegraph_place_id')

# save merged location and foot traffic data
saveRDS(footTraffic, file = 'D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/00-merged-location-and-foot-traffic.rds')




##### top category data #####

# split data by location category
footTrafficByCategory <- split(footTraffic, footTraffic$top_category)

# sum foot traffic by month for each category
footTrafficByCategoryAggregate <- lapply(footTrafficByCategory, function(x){ aggregate(x$raw_visit_counts, list(Date = x$date_range_start), sum) })

# order data by date
footTrafficByCategoryAggregate <- lapply(footTrafficByCategoryAggregate, function(dat){ dat[order(dat$Date),] })

# remove first two rows (first two months) because of low counts
footTrafficByCategoryAggregate <- lapply(footTrafficByCategoryAggregate, function(dat){ dat[-c(1,2),] })

# remove list elements with 0 rows of data
footTrafficByCategoryAggregate <- footTrafficByCategoryAggregate[sapply(footTrafficByCategoryAggregate, function(x){nrow(x) > 0})]

# divide by initial value to get relative change
footTrafficByCategoryAggregateRelativeValue <- lapply(footTrafficByCategoryAggregate, function(dat){ dat$x <- dat$x / dat$x[[1]]; dat })

# keep only the top 5 categories
topCategories <- c('Clothing Stores', 'Museums, Historical Sites, and Similar Institutions', 'Personal Care Services', 'Religious Organizations', 'Restaurants and Other Eating Places')
footTrafficByCategoryAggregate              <- footTrafficByCategoryAggregate[names(footTrafficByCategoryAggregate)                           %in% topCategories]
footTrafficByCategoryAggregateRelativeValue <- footTrafficByCategoryAggregateRelativeValue[names(footTrafficByCategoryAggregateRelativeValue) %in% topCategories]

# add column of category
for(i in 1:length(footTrafficByCategoryAggregate             )){ footTrafficByCategoryAggregate             [[i]]$Category = topCategories[[i]] }
for(i in 1:length(footTrafficByCategoryAggregateRelativeValue)){ footTrafficByCategoryAggregateRelativeValue[[i]]$Category = topCategories[[i]] }

# combine data
footTrafficByCategoryAggregate              <- do.call(rbind, footTrafficByCategoryAggregate)
footTrafficByCategoryAggregateRelativeValue <- do.call(rbind, footTrafficByCategoryAggregateRelativeValue)

# add total relative trend
footTrafficAggregateRelativeValue <- aggregate(footTraffic$raw_visit_counts, list(Date = footTraffic$date_range_start), sum)
footTrafficAggregateRelativeValue <- footTrafficAggregateRelativeValue[-c(1,2),]
footTrafficAggregateRelativeValue$x <- footTrafficAggregateRelativeValue$x / footTrafficAggregateRelativeValue$x[[1]]
footTrafficAggregateRelativeValue$Category <- footTrafficAggregateRelativeValue$label <- 'Total'
footTrafficAggregateRelativeValue$label    <- as.factor(footTrafficAggregateRelativeValue$label)




##### plot data #####

# create label variable
categoryLabels <- data.frame(Category = c('Clothing Stores', 'Museums, Historical Sites, and Similar Institutions', 'Personal Care Services', 'Religious Organizations', 'Restaurants and Other Eating Places'),
                             label    = factor(c('Clothing Stores', 'Museums, Historical Sites,\nand Similar Institutions', 'Personal Care Services', 'Religious Organizations', 'Restaurants and Other\nEating Places')))
footTrafficByCategoryAggregate              <- dplyr::left_join(footTrafficByCategoryAggregate,              categoryLabels, by = 'Category')
footTrafficByCategoryAggregateRelativeValue <- dplyr::left_join(footTrafficByCategoryAggregateRelativeValue, categoryLabels, by = 'Category')
footTrafficByCategoryAggregateRelativeValue <- rbind(footTrafficByCategoryAggregateRelativeValue, footTrafficAggregateRelativeValue)

# total count
ggplot(data = footTrafficByCategoryAggregate) + geom_line(aes(x = Date, y = x, color = label), size = 0.8) +
  labs(y = 'Total visit counts') + theme(text = element_text(size = 20)) +
  scale_x_datetime(date_breaks = '4 months', labels = date_format('%Y-%m')) +
  scale_color_discrete(name = 'Category') +
  theme(legend.key.size = unit(3, 'lines'))

ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateTopFiveCategoriesTimeTrend.png',
       dpi = 300, height = 5, width = 12)

# relative values
ggplot(data = footTrafficByCategoryAggregateRelativeValue) + geom_line(aes(x = Date, y = x, color = label), size = 0.8) +
  labs(y = 'Relative total visit counts') + theme(text = element_text(size = 20)) +
  scale_x_datetime(date_breaks = '4 months', labels = date_format('%Y-%m')) +
  scale_color_discrete(name = 'Category') +
  theme(legend.key.size = unit(3, 'lines'))

ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/LineGraphs/aggregateTopFiveCategoriesTimeTrendRelative.png',
       dpi = 300, height = 5, width = 12)




# are there more parks in Aug and Sep 2019 causing the spike in visits?
numParksMuseumsByMonth <- data.table::setDT(dat)[, .(count = data.table::uniqueN(location_name)), by = date_range_start]
ggplot(data = numParksMuseumsByMonth) +
  geom_line(aes(x = date_range_start, y = count))+
  labs(x = 'Date', y = 'Count') +
  theme(text = element_text(size = 15))
