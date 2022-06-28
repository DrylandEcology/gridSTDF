rm(list=ls(all=TRUE))
library(stars)

gm_wUS_nc <- stars::read_ncdf('weather_grid_fix/grid_westernUS.nc', var = "ID")
sf::st_crs(gm_wUS_nc) <- "OGC:CRS84"
plot(gm_wUS_nc)

gm_wUS_poly <- sf::st_as_sf(gm_wUS_nc, as_points = FALSE)
gm_wUS_poly$geom_centroid <- sf::st_geometry(sf::st_centroid(gm_wUS_poly))
gm_wUS_poly <- cbind(gm_wUS_poly, do.call(rbind,lapply(gm_wUS_poly$geometry, st_bbox)))

gm_wUS_poly <- order(gm_wUS_poly, ymin)
#gm_wUS_poly$geom_xmin <- bbox[,1]
#gm_wUS_poly$geom_ymin <- bbox[,2]

