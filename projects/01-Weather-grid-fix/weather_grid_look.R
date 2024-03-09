rm(list=ls(all=TRUE))
library(stars)

gm_wUS_nc <- stars::read_ncdf('./projects/01-Weather-grid-fix/grid_westernUS.nc', var = "ID")
sf::st_crs(gm_wUS_nc) <- "OGC:CRS84"
plot(gm_wUS_nc, reset = FALSE)

gm_wUS_poly <- sf::st_as_sf(gm_wUS_nc, as_points = FALSE)
gm_wUS_poly$geom_centroid <- sf::st_geometry(sf::st_centroid(gm_wUS_poly))
gm_wUS_poly <- cbind(gm_wUS_poly, do.call(rbind,lapply(gm_wUS_poly$geometry, st_bbox)))

# try intersectong .. and build a lookup table. Add the CD Region tbl_sites
# Test...
CD102 <- shapefile(x = 'CD102/CD102.shp')
plot(CD102, add = TRUE, col = 'blue')

Lat <- unique(gm_wUS_poly$ymin)[2]
Long <-  unique(gm_wUS_poly$xmin)[7]
points <- data.frame(x = Long, 
                     y = Lat)
coordinates(points) <- ~ x + y
proj4string(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
CDRegion <- as.numeric(over(points, CD102)$ID4)

plot(points, add = T, col = 'red')

