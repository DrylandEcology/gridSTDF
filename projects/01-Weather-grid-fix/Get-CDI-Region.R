# Find the overlapping CD Regions
# Method 1 - Get center of gridMet cell and choose that which overlaps
# Method 2 - For those that are missing - Use whole cell to see where cell and polygon overlap ..
# # # # 1102 of 3919 further ided
rm(list=ls(all=TRUE))
library(stars)
library(sf)
library(tidyverse)
sf_use_s2(FALSE)

CD102 <- sf::st_read('main/CD102/CD102.shp')

# Get grid
gm_wUS_nc <- stars::read_ncdf('projects/01-Weather-grid-fix/grid_westernUS.nc', var = "ID")
sf::st_crs(gm_wUS_nc) <- "OGC:CRS84"
gm_wUS_poly <- sf::st_as_sf(gm_wUS_nc, as_points = FALSE)

# find centroids 
gm_wUS_poly$geom_centroid <- sf::st_geometry(sf::st_centroid(gm_wUS_poly))
gm_wUS_poly <- cbind(gm_wUS_poly, do.call(rbind,lapply(gm_wUS_poly$geometry, st_bbox)))
gm_wUS_poly <- cbind(gm_wUS_poly,  do.call(rbind, st_geometry(gm_wUS_poly$geom_centroid)) %>% 
                       as_tibble() %>% setNames(c("Longitude","Latitude")))

# Find intersection of centroids / points!
points <- gm_wUS_poly[,c("Longitude","Latitude")]
points2 <- st_geometry(gm_wUS_poly$geom_centroid)

points$region <- (apply(st_intersects(CD102, points2, sparse = FALSE), 2, 
                     function(col) { 
                       CD102[which(col), ]$ID4
                     }))

points$region2 <- as.numeric(points$region)

points <- points %>%
  rowwise() %>%
  mutate(point = list(st_point(c(Longitude,Latitude))))

# Method 2 ---------------------------------------------------------------------

# Which are missing?
Missing <- points[is.na(points$region2),]
points <- points[!is.na(points$region2),] # not missing

# Join back up with the gm_wUS polygon .....
Missing2 <- as.data.frame(Missing[,c('Latitude', 'Longitude')])
Missing2 <- merge(Missing2, gm_wUS_poly)


# Find missing by intersection of whole polygon ...
Missing2$region <- (apply(st_intersects(CD102, Missing2$geometry, sparse = FALSE), 2, 
                        function(col) { 
                          CD102[which(col), ]$ID4
                        }))


Missing2$region <- ifelse(Missing2$region == 'character(0)', 0, Missing2$region)
Multiples <- Missing2[lengths(Missing2$region)>1, ] # save multis for another steps

Missing2 <- Missing2[lengths(Missing2$region)==1, ]
Missing2$region2 <- as.numeric(Missing2$region)

Missing3 <- Missing2[Missing2$region2 == 0,] 
Missing2 <- Missing2[Missing2$region2 > 0,] # 1102 of 3919 further ided (2817 still missing :())


#plot --- 
Missing_pnts <- do.call("st_sfc",c(lapply(1:nrow(Missing3),
                                          function(i) {st_point(as.numeric(Missing3[i, c(2,1)]))}), 
                                   list("crs" = 4326)))

Mult_pnts <- do.call("st_sfc",c(lapply(1:nrow(Multiples),
                                          function(i) {st_point(as.numeric(Multiples[i, c(2,1)]))}), 
                                   list("crs" = 4326)))
ggplot() +
  geom_sf(data = CD102) +
  geom_sf(data = Mult_pnts)


# Final step --------------------- combine and save
x <-  as.data.frame(points[,c('Longitude', 'Latitude', 'region2')])
x2 <- as.data.frame(Missing2[,c('Longitude', 'Latitude', 'geometry' ,'region2')])

x3 <- rbind(x, x2)

# Save to site file
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))

Sites <- merge(Sites, as.data.frame(x3[,c('Longitude', 'Latitude', 'region2')]))

write.csv(Sites, "main/Data/WeatherDBSitesTable_WestIndex2.csv", row.names = FALSE)

 summary(Sites)
