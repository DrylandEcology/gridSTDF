rm(list=ls(all=TRUE))
library(stars)
library(sf)
sf_use_s2(FALSE)

CD102 <- sf::st_read('main/CD102/CD102.shp')

# Get and center grid!
gm_wUS_nc <- stars::read_ncdf('projects/01-Weather-grid-fix/grid_westernUS.nc', var = "ID")
sf::st_crs(gm_wUS_nc) <- "OGC:CRS84"

gm_wUS_poly <- sf::st_as_sf(gm_wUS_nc, as_points = FALSE)
gm_wUS_poly$geom_centroid <- sf::st_geometry(sf::st_centroid(gm_wUS_poly))
gm_wUS_poly <- cbind(gm_wUS_poly, do.call(rbind,lapply(gm_wUS_poly$geometry, st_bbox)))

#gm_wUS_poly <- order(gm_wUS_poly, ymin)
#gm_wUS_poly$geom_xmin <- bbox[,1]
#gm_wUS_poly$geom_ymin <- bbox[,2]


# Find intersection!
points <- do.call(rbind, st_geometry(gm_wUS_poly$geom_centroid)) %>% 
  as_tibble() %>% setNames(c("Longitude","Latitude"))

points2 <- st_geometry(gm_wUS_poly$geom_centroid)

points$region <- (apply(st_intersects(CD102, points2, sparse = FALSE), 2, 
                     function(col) { 
                       CD102[which(col), ]$ID4
                     }))

points$region2 <- as.numeric(points$region)

points <- points %>%
  rowwise() %>%
  mutate(point = list(st_point(c(Longitude,Latitude))))


# Which are missing?
Missing <- points[is.na(points$region2),]
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(Missing),
                                     function(i) {st_point(as.numeric(Missing[i, 1:2]))}), list("crs" = 4326)))

table(Missing$Latitude)

ggplot() +
  geom_sf(data = CD102) +
  geom_sf(data = pnts_sf)


# Save to site file
x <-  as.data.frame(points[,c('Longitude', 'Latitude', 'region2')])
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))

Sites <- merge(Sites, as.data.frame(points[,c('Longitude', 'Latitude', 'region2')]))

write.csv(Sites, "main/Data/WeatherDBSitesTable_WestIndex.csv", row.names = FALSE)

summary(Sites)
