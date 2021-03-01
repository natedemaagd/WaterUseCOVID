
library(doParallel)
registerDoParallel(cores = 8)


#### load data ----

# load panel data
dat <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/00-merged-location-and-foot-traffic.rds')

# load locations data and keep only unique lat/lon and in Hawaii data
folders   <- list.dirs(path = 'D:/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Locations')[-1]
filenames <- lapply(folders, function(f){ list.files(path = f, pattern = 'core_poi')})
files     <- foreach(i = 1:length(folders), .combine = 'rbind') %dopar% {

  locations <- list()
  for(j in 1:length(filenames[[i]])){

    # read data, remove duplicates, and keep only locations for which we have foot traffic data
    df <- readr::read_csv(paste0(folders[[i]], '/', filenames[[i]][[j]]), col_names = TRUE)
    df <- df[!duplicated(df$safegraph_place_id), c('safegraph_place_id', 'latitude', 'longitude')]
    df <- df[df$safegraph_place_id %in% dat$safegraph_place_id,]
    locations[[j]] <- as.data.frame(df)
    rm(df)

  }

  return(locations)

}

# combine locations and remove duplicates
locations <- do.call('rbind', files)
locations <- locations[!duplicated(locations$safegraph_place_id),]




#### merge latitude and longitude ----

dat <- dplyr::left_join(dat, locations, 'safegraph_place_id')
dat <- dat[!is.na(dat$longitude),]

# save data
saveRDS(dat, file = "D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/02-SafeGraph-with-LatLon.rds")
