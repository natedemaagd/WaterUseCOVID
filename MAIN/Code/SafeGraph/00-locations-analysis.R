


##### setup #####

# load packages
library(ggplot2); library(viridis); library(rgdal); library(ColorByDensity)

# load data
filesList <- list.files("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Raw/SafeGraph/CorePlaces/2020_08", pattern = 'core')
filesList <- lapply(filesList, function(x){ read.csv(paste0("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Raw/SafeGraph/CorePlaces/2020_08/", x)) })




##### format data #####

# rowbind all data.frames
locations <- do.call(rbind, filesList)

# remove any repeat locations
locations <- locations[!duplicated(locations$safegraph_place_id),]

# remove non-Hawaii locations
locations <- locations[locations$region == 'HI',]

# reduce further: keep only Oahu locations by lat/lon
locations <- locations[locations$latitude >= 21.248808 & locations$latitude <= 21.719069 & locations$longitude >= -158.287189 & locations$longitude <= -157.643533,]

rm(filesList)




##### plot location density #####

# get density value
locations$location_density <- get_point_density(locations$longitude, locations$latitude, n = 100)

# load Oahu outline
oahuOutline <- readOGR("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Common data/Oahu coastline shapefile/Coastline.shp")


# plot locations and color by density
ggplot() + geom_polygon(data = oahuOutline, aes(long, lat, group = group), color = 'black', fill = 'gray90') +
  geom_point(data = locations, aes(x = longitude, y = latitude, color = location_density)) + scale_color_viridis(name = 'Location density',
                                                                                                                     breaks = c(min(locations$location_density),
                                                                                                                                max(locations$location_density)),
                                                                                                                     labels = c('Low', 'High')) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.background = element_blank(), axis.ticks = element_blank())


ggsave(filename = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/Maps/point-of-interest-density.png', dpi = 300, width = 8, height = 5)




##### plot location type #####

# create table of location type
location_types <- as.data.frame(table(locations$top_category))
colnames(location_types) <- c('Category', 'Number')
location_types$Category <- as.character(location_types$Category)

# get top 5 categories
location_types_top5 <- dplyr::slice_max(location_types, order_by = Number, n = 5)

# if not in top 5 categories, replace type with 'Other'
locations$category_label <- ifelse(locations$top_category %in% location_types_top5$Category, locations$top_category, 'Other')

# plot locations and color by type
ggplot() + geom_polygon(data = oahuOutline, aes(long, lat, group = group), color = 'black', fill = 'gray90') +
  geom_point(data = locations, aes(x = longitude, y = latitude, color = category_label), alpha = 0.5) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())

ggsave(filename = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/Maps/point-of-interest-TopCategories.png', dpi = 300, width = 8, height = 5)

# plot locations and color by type (no 'other' category')
ggplot() + geom_polygon(data = oahuOutline, aes(long, lat, group = group), color = 'black', fill = 'gray90') +
  geom_point(data = locations[locations$category_label != 'Other',], aes(x = longitude, y = latitude, color = category_label), alpha = 0.5) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())

ggsave(filename = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/WaterUseCOVID/MAIN/Tables and figures/Maps/point-of-interest-TopCategories-NoOther.png', dpi = 300, width = 8, height = 5)




##### save data
saveRDS(locations, file = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/SafeGraphHawaiiLocations.rds')



