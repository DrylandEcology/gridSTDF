# This script inputs the sagebrush biome polygon, makes sure it is in the same projection as the gridMet data, and saves the location in .csv format
# Alice Stears
# 08 February 2025


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(sf)

# Read in sagebrush biome outline --------------------------------------------
poly <- sf::st_read("./main/Data/SagebrushBiomeExtent/", "US_Sagebrush_Biome_2019") %>% 
  sf::st_transform(sf::st_crs("EPSG:4326"))

# determine which grid-cells overlay this polygon --------------------------
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))

Sites_sfc <- st_sfc(lapply(1:nrow(Sites), FUN = function(x) {
  #st_point(c(x[2], x[1]))
  st_point(c(Sites[x,"Longitude"], Sites[x,"Latitude"]))
}))
Sites_sf <- st_sf(Sites, geometry = Sites_sfc, crs = sf::st_crs("EPSG:4326"))

# Find the centroids that overlap with the bounding polygon 
whichSites <- Sites_sf[st_intersects(poly, Sites_sf)[[1]],] %>% 
  st_drop_geometry()



# Save Output -------------------------------------------------------------
write.csv(whichSites, "./main/Data/SagebrushBiomeList.csv")


