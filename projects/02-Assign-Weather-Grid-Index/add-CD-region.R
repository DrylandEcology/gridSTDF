library(data.table)

Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
CDRegion <- as.data.frame(data.table::fread("projects/01-Weather-grid-fix/CD_region_grid.csv"))

Sites <- merge(Sites, as.data.frame(CDRegion[,c('Longitude', 'Latitude', 'region2')]))

write.csv(Sites, "main/Data/WeatherDBSitesTable_WestIndex.csv", row.names = FALSE)

summary(Sites)
