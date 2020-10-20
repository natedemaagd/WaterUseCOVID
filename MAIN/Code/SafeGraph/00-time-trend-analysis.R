

# load Hawaii foot traffic data and combine
footTraffic2018 <- readRDS("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Monthly places patterns (foot traffic - Hawaii only)/2018/wholeYear2018.rds")
footTraffic2019 <- readRDS("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Monthly places patterns (foot traffic - Hawaii only)/2019/wholeYear2019.rds")
footTraffic2020 <- readRDS("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Data/SafeGraph/Monthly places patterns (foot traffic - Hawaii only)/2020/wholeYear2020.rds")

footTraffic <- do.call('rbind', list(footTraffic2018, footTraffic2019, footTraffic2020))

rm(footTraffic2018, footTraffic2019, footTraffic2020)


# load locations data
locations <- readRDS("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Intermediate/SafeGraphHawaiiLocations.rds")
