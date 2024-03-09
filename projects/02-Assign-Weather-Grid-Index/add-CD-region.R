library(data.table)

Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
CDRegion <- as.data.frame(data.table::fread("projects/01-Weather-grid-fix/CD_region_grid.csv"))

CD102 <- sf::st_read('main/CD102/CD102.shp')

## temporarily make d.fs into scf objects
Sites_sfc <- do.call("st_sfc",c(lapply(1:nrow(Sites),
                                          function(i) {st_point(as.numeric(Sites[i, c(3,2)]))}), 
                                   list("crs" = 4326)))
Sites_sfc <- st_sf(Sites, geometry = Sites_sfc, crs = "EPSG:4326")

CDRegion_sfc <- do.call("st_sfc",c(lapply(1:nrow(CDRegion),
                                       function(i) {st_point(as.numeric(CDRegion[i, c(1,2)]))}), 
                                list("crs" = 4326)))

# add the two together 
testMerge <- left_join(Sites_sfc, CDRegion)

# testTest <- st_intersection(CD102[CD102$ID4 == 79,], testMerge) %>% 
#   filter(region2 == 79)
# #Sites_2 <- merge(Sites_sfc, as.data.frame(CDRegion_sfc[,c('Longitude', 'Latitude', 'region2')]))
# testBad <- st_intersection(CD102[CD102$ID4 == 79,], testMerge) %>% 
#   filter(is.na(region2))
# 
# ggplot() + 
#   geom_sf(data = CD102[CD102$ID4 == 79,]) + 
#   #geom_sf(data = st_intersection(CD102[CD102$ID4 == 79,], Sites_sfc), col = "blue", alpha = .5) + 
#   #geom_sf(data = st_intersection(CD102[CD102$ID4 == 79,], CDRegion_sfc), col = "red", alpha = .5) +
#   geom_sf(data = st_intersection(CD102[CD102$ID4 == 79,], testMerge), aes(col = region2),  alpha = .9)
#   #geom_sf(data = CDRegion_sfc)
# 
# mapview(CD102[CD102$ID4 == 79,]) + mapview(testTest) + mapview(testBad, col.regions = "green")

# issue: some grid cells are being dropped, I think because of slightly different centroids? try rounding the lat/lons? 
# round lat and long in the two d.fs 

# try joining spatially? 
Sites_sfc$region2 <- NA
testIntersects <- st_intersects(CD102, Sites_sfc)
 
Site_joins <- lapply(1:length(testIntersects), function(x) {
    IDs <- testIntersects[[x]]
    temp <- Sites_sfc[IDs, ]
    if(nrow(temp)>0) {
      temp$region2 <- as.numeric(st_drop_geometry(CD102[x,"ID4"]))
    }
    return(temp)
  }
)
Site_joins <- purrr::list_rbind(Site_joins) %>% 
  st_as_sf() %>%
  st_drop_geometry()

write.csv(Site_joins, "main/Data/WeatherDBSitesTable_WestIndex.csv", row.names = FALSE)

