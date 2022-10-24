### SHELL> mpiexec -np 4 Rscript --vanilla implementation/Simple_weather_pbdmpi_test.R
rm(list=ls(all=TRUE))

library(pbdMPI, quiet = TRUE)
library(pbdNCDF4, quiet = TRUE)
library(lubridate, quiet = TRUE)
library(rSOILWAT2, quiet = TRUE)

if(!interactive()) init()

source('functions/weatherFunctions.R')
source('functions/netcdf_functions.R')


################### ----------------------------------------------------------------
# Part 0 - Setup
################### ----------------------------------------------------------------

# MPI Parameters ---------------------------------------------------------------
rank <- comm.rank() # processor's rank
size <- comm.size() # total processors (i.e. equal to tasks in the SLURM scripts)
comm.print(size)

n.workers <- size - 1 # reserve one for other activities

alljid <- get.jid(n = 1000, method = "block", all = FALSE) 
comm.print(alljid)

# create netCDFs in parallel to write to:
source('projects/03-Make-Climatologies-netCDF/Create-template-netCDFs.R')

################### ----------------------------------------------------------------
# Part 1 - Getting and formatting historical weather data
################### ----------------------------------------------------------------

# Get Weather and Site Info  ------------------------------------------------------------------------
weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3')
#Sites <- rSOILWAT2::dbW_getSiteTable()
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))

for (i in alljid) { # use while not for
  
  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')

  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  LatIdx <- Sites$LatIndex[i]
  LonIdx <- Sites$LonIndex[i]

  st <- c(LatIdx, LonIdx, 1)
  co <- c(1, 1, 365)
  comm.print(st)

  if(!interactive()) comm.print(paste('Site', Site_id, 'running'))
  # if(!interactive()) comm.print(LonIdx)

  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  ids <- rSOILWAT2:::select_years(years, 1991, 2020)
  wdata <- wdata[ids]
  
  # get climatology
  
  
  
  wdata_currYear <- wdata_currYear[[1]]
  wdata_currYear <- rSOILWAT2::dbW_dataframe_to_weatherData(wdata_currYear[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)

  wdata <- c(wdata, wdata_currYear)
  rSOILWAT2::dbW_disconnectConnection()

  # ---------- Outputs --------------------------------------------------------
  wdata_2022 <- wdata[['2022']]
  wdata_2022_tmax <- as.vector(wdata_2022@data[,2])

### write variable values to file

  ncvar_put(tmmx_nc, "tmmx", wdata_2022_tmax, start = st, count = co)
  nc_sync(tmmx_nc) 
    # Another netCDF that tracks success and failure

}

### close file
on.exit(pbdNCDF4::nc_close(nc))


################### ----------------------------------------------------------------
# Part X - End and check
################### ----------------------------------------------------------------

# # Parallel open
# nc <- nc_open_par("test_maxtemp.nc")
# nc_var_par_access(nc, "testMatrix")

# ### get data dimension.
# n <- length(nc$dim$rows$vals)
# p <- length(nc$dim$columns$vals)

# ### read variable values to file
# x <- ncvar_get(nc, "testMatrix", start = st, count = co)

# ### Print results.
# comm.cat("n = ", n, " p = ", p, "\n", sep = "")
# comm.print(x, all.rank = TRUE)

# ### close file
# nc_close(nc)
# ncdump("test_maxtemp.nc")

 if(!interactive()) {

#   #info.free()
   barrier()
   finalize()
 }

