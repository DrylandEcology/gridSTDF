### SHELL> mpiexec -np 4 Rscript --vanilla implementation/Simple_weather_pbdmpi_test.R
rm(list=ls(all=TRUE))

library(pbdMPI, quiet = TRUE)
library(RNetCDF, quiet = TRUE)
library(lubridate, quiet = TRUE)
#library(raster)

if(!interactive()) init()

source('functions/weatherFunctions.R')

################### ----------------------------------------------------------------
# Part 0 - Setup
################### ----------------------------------------------------------------

# MPI Parameters ---------------------------------------------------------------

if(!interactive()) {
  
  rank <- comm.rank()
  size <- comm.size()
  hostname <- spmd.get.processor.name()
  n.workers <- size - 1
  comm.print(n.workers)
  
  alljid <- get.jid(n = 50, method = "block", all = FALSE) 
  comm.print(alljid)
}

# NetCDF-------------------------------------------------------------------------
numRows <- 715 # nrows, longitude?
numCols <- 567 #ncols, latitude?
days <- 365 # 3 years worth of days!

filename <- "test_weather.nc"
info.create() 
ncid <- create.nc(filename, format="netcdf4", clobber = TRUE, mpi_comm=comm.c2f(), mpi_info=info.c2f())
rdim <- dim.def.nc(ncid, "rows", numRows)
cdim <- dim.def.nc(ncid, "cols", numCols)
tdim <- dim.def.nc(ncid, "time", days)
varid <- var.def.nc(ncid, "MaxTemp", "NC_FLOAT", c(rdim, cdim, tdim))



# Get Weather and Site Info  ------------------------------------------------------------------------
weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3')
#Sites <- rSOILWAT2::dbW_getSiteTable()
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))


################### ----------------------------------------------------------------
# Part 1 - Simulation
################### ----------------------------------------------------------------

for (i in alljid) { # use while not for

  # #if(i %in% seq(1, nrow(Sites), 10)) print(i)
  
  # ################### ------------------------------------------------------------
  # # Part 1 - Getting and formatting historical weather data
  # ################### ------------------------------------------------------------

  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')

  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  LatIdx <- Sites$LatIndex[i]
  LonIdx <- Sites$LonIndex[i]

  # if(!interactive()) comm.print(paste('Site', Site_id, 'running'))
  # if(!interactive()) comm.print(LonIdx)

  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  years <- rSOILWAT2::get_years_from_weatherData(wdata)
  ids <- rSOILWAT2:::select_years(years, 1990, 2021)
  wdata <- wdata[ids]

  currYear <- lubridate::year(Sys.Date())
  wdata_2021_plus <- getWeatherData(Lat, Long, currYear,
                                    dir = 'main/Data/www.northwestknowledge.net/metdata/data/')

  wdata_2021_plus <- wdata_2021_plus[[1]]
  wdata_2021_plus <- rSOILWAT2::dbW_dataframe_to_weatherData(wdata_2021_plus[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)

  wdata <- c(wdata, wdata_2021_plus)
  DBI::bDisconnect(wdata)

  # ---------- Outputs --------------------------------------------------------
  wdata_2021 <- wdata[['2021']]

  var.put.nc(ncfile = ncid,
           variable = varid,
           data  = wdata_2021@data[,'Tmax_C'], # the data
           start=c(LonIdx, LatIdx, 1),
           count=c(1, 1, days))
  # Another netCDF that tracks success and failure
}


# Shut down MPI ---------------------------------------------------------------
if(!interactive()) {

  #close.nc(ncid)
  info.free()
  barrier()
  finalize()
}

