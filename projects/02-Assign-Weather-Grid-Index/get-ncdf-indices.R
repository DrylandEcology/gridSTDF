rm(list = ls(all = TRUE))

library(RNetCDF)

#file <- "main/Data/www.northwestknowledge.net/metdata/data/tmmx_2022.nc"
file1 <- "projects/03-Make-Climatologies-netCDF/west.temp.nc1"

# NetCDF -----------------------------------------------------------------------
temp.nc <- open.nc(file1)
print.nc(temp.nc)

# get
lat <- var.get.nc(temp.nc,"lat")
#lat <- rev(lat)
lon <- var.get.nc(temp.nc,"lon")


data <- var.get.nc(temp.nc,"air_temperature")
data <- data[, ncol(data):1,  ] #lat being our dimension number 2
data_day1 <- data[, , 1]

image(lon, lat, data_day1)

# WeatherDB  -------------------------------------------------------------------

weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')
Sites <- rSOILWAT2::dbW_getSiteTable()
dim(Sites)

# Function to find lat and long index ------------------------------------------

findCoordIndex <- function(value, listOfCoords) {
  
  nearest <- which(abs(listOfCoords - value) == min(abs(listOfCoords - value)))

  return(nearest)
  
}

# Loop and get  -------------------------------------------------------------

Sites$LatIndex <- NA
Sites$LonIndex <- NA

for(i in 1:nrow(Sites)) {
  
  if(i %in% seq(1, nrow(Sites), 1000)) print(i)
  
  Sites$LatIndex[i] <- findCoordIndex(Sites$Latitude[i], lat)
  Sites$LonIndex[i] <- findCoordIndex(Sites$Longitude[i], lon)
  
}

write.csv(Sites, "main/Data/WeatherDBSitesTable_WestIndex.csv", row.names = FALSE)
summary(Sites)

# ADd CD Region!!




# old
# # western_region.nc1 <- rSW2st::read_netCDF(file1, method = "array", 
# xy_names = c("lon", "lat"), 
# time_name = "day")
# lon_west <- lon[1:715]
# lat_west <- lat[19:585]
# 
# data_day1_west <- data[1:715, 19:585, 1]
# image(lon_west, lat_west, data_day1_west)
# 
# data_west <- var.get.nc(temp.nc, 
#                         "air_temperature", 
#                         start=c(1, 19, 1), # LonStartIdx, LatStartIdx
#                         count=c(715, 567, 365))
# 
# data_day1_west <- data[1:715, 19:585, 1]
# image(lon_west, lat_west, data_day1_west)
