rm(list=ls(all=TRUE))

library(pbdMPI, quiet = TRUE)
library(pbdNCDF4, quiet = TRUE)
library(RNetCDF, quiet = TRUE)
library(lubridate, quiet = TRUE)

source('functions/netcdf_functions_HPC.R')


################### ----------------------------------------------------------------
# Part 0 - Setup
################### ----------------------------------------------------------------

Sites <- Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

source('projects/05-Setup-futureMonthly-netCDFs/Create-template-netCDFs.R')

sites_for_now <- 15000:15050 
sites <- 1:(dim(Sites)[1]/10)


for (i in sites) { # use while not for
  
  if(i %in% seq(1, nrow(Sites), 10000)) print(i)
  
  
  ################### ------------------------------------------------------------
  # Part 1 - Getting and formatting historical weather data 
  ################### ------------------------------------------------------------
  
  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')
  
  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  LatIdx <- Sites$LatIndex[i]
  LonIdx <- Sites$LonIndex[i]

  st <- c(LonIdx, LatIdx, 1)
  co <- c(1, 1, 1)

  # get data
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  
  # put
  vals <- as.vector(wdata[['2021']]@data[1,2])
 
  #write!
  pbdNCDF4::ncvar_put(tmean_median, "ta", vals, 
                      start = st, count = co)
  nc_sync(tmean_median) 
}


# look at it



break
nc_close(tmean_median)

file <- "projects/05-Setup-futureMonthly-netCDFs/Outputs/Test_20230605/tmean_dy_gridSTDF_median-prediction_062023.nc"

# NetCDF -----------------------------------------------------------------------
temp.nc <- open.nc(file)
print.nc(temp.nc)

# get
lat <- var.get.nc(temp.nc,"lat")
lat <- rev(lat)
lon <- var.get.nc(temp.nc,"lon")


data <- var.get.nc(temp.nc,"ta")
data <- data[, ncol(data):1,  ] #lat being our dimension number 2
data_day1 <- data[, , 1]

image(lon, lat, data_day1, ylim = c(50,25))
  
  
  