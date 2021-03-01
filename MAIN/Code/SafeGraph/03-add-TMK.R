

library(rgdal); library(maptools); library(spatialEco); library(sf)


# load data
dat <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/02-SafeGraph-with-LatLon.rds")
shp_tmk <- readOGR('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/OahuTMK', layer = 'oahtmk')
shp_tmk <- spTransform(shp_tmk, CRS("+proj=longlat +datum=WGS84"))

# convert dat into spatial object
dat_sp <- st_as_sf(dat, coords = c("longitude", "latitude"))
st_crs(dat_sp) <- 4326

shp_tmk <- st_as_sf(shp_tmk)
st_crs(shp_tmk) <- 4326

# sort locations into TMK polygons
tmk_locations <- spatialEco::point.in.poly(dat_sp, shp_tmk)
dat <- as.data.frame(tmk_locations)

# remove unneeded columns
dat <- dat[c("safegraph_place_id", "date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "median_dwell", "parent_safegraph_place_id", "top_category", "sub_category",
             "category_label", "TMK", "TMK9TXT", "TMK8NUM", "TMK9NUM", "coords.x1", "coords.x2")]

# save data
saveRDS(dat, file = "D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/03-SafeGraph-with-tmk.rds")
