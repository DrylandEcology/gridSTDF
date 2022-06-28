rm(list = ls(all = TRUE))

library(RNetCDF)

file <- "Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc"

# NetCDF -----------------------------------------------------------------------
temp.nc <- open.nc(file)
print.nc(temp.nc)

# get
lat <- var.get.nc(temp.nc,"lat")
lat <- rev(lat)
lon <- var.get.nc(temp.nc,"lon")


data <- var.get.nc(temp.nc,"air_temperature")
data <- data[, ncol(data):1,  ] #lat being our dimension number 2
data_day1 <- data[, , 1]

image(lon, lat, data_day1)

# WeatherDB  -------------------------------------------------------------------

weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3')
Sites <- rSOILWAT2::dbW_getSiteTable()
dim(Sites)

# Function to find lat and long index ------------------------------------------

findCoordIndex <- function(value, listOfCoords) {
  
  nearest <- which(abs(listOfCoords - value) == min(abs(listOfCoords - value)))

  return(nearest)
  
}

findCoordIndex(value, lat)
findCoordIndex(value, lon)

# Loop and get ... -------------------------------------------------------------

Sites$LatIndex <- NA
Sites$LonIndex <- NA

for(i in 1:nrow(Sites)) {
  
  if(i %in% seq(1, nrow(Sites), 1000)) print(i)
  
  Sites$LatIndex[i] <- findCoordIndex(Sites$Latitude[i], lat)
  Sites$LonIndex[i] <- findCoordIndex(Sites$Longitude[i], lon)
  
}

write.csv(Sites, "Data/WeatherDBSitesTable.csv", row.names = FALSE)
summary(Sites)

# Should I subset the netCDF to Western US and then loop and get index again? --
lon_west <- lon[1:715]
lat_west <- lat[19:585]

data_day1_west <- data[1:715, 19:585, 1]
image(lon_west, lat_west, data_day1_west)

data_west <- var.get.nc(temp.nc, 
                        "air_temperature", 
                        start=c(1, 19, 1), # LonStartIdx, LatStartIdx
                        count=c(715, 567, 365))

data_day1_west <- data[1:715, 19:585, 1]
image(lon_west, lat_west, data_day1_west)

# Loop and get again -------------------------------------------------------------

Sites$LatIndex <- NA
Sites$LonIndex <- NA

for(i in 1:nrow(Sites)) {
  
  if(i %in% seq(1, nrow(Sites), 1000)) print(i)
  
  Sites$LatIndex[i] <- findCoordIndex(Sites$Latitude[i], lat_west)
  Sites$LonIndex[i] <- findCoordIndex(Sites$Longitude[i], lon_west)
  
}

write.csv(Sites, "Data/WeatherDBSitesTable_WestIndex.csv", row.names = FALSE)
summary(Sites)


