### SHELL> mpiexec -np 4 Rscript --vanilla implementation/Simple_weather_pbdmpi_test.R
rm(list=ls(all=TRUE))

library(pbdMPI, quiet = TRUE)
library(pbdNCDF4, quiet = TRUE)
library(lubridate, quiet = TRUE)
library(rSOILWAT2, quiet = TRUE)

if(!interactive()) init()

source('functions/weatherFunctions.R')
source('functions/netcdf_functions2.R')


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

  st <- c(LonIdx, LatIdx, 1)
  co <- c(1, 1, 365)
  comm.print(st)

  if(!interactive()) comm.print(paste('Site', Site_id, 'running'))
  # if(!interactive()) comm.print(LonIdx)

  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  years <- rSOILWAT2::get_years_from_weatherData(wdata)
  ids <- rSOILWAT2:::select_years(years, 1991, 2020)
  wdata <- wdata[ids]
  wdata <- rSOILWAT2::dbW_weatherData_to_dataframe(wdata)
  
  rSOILWAT2::dbW_disconnectConnection()
  
  # get climatology
  tmmx <- aggregate(wdata[,3], list(wdata[,2]), FUN=mean) 
  tmmx <- tmmx$x[1:365]

  tmmn <- aggregate(wdata[,4], list(wdata[,2]), FUN=mean) 
  tmmn <- tmmn$x[1:365]

  pr <- aggregate(wdata[,5], list(wdata[,2]), FUN=mean) 
  pr <- pr$x[1:365]


  # ---------- Outputs --------------------------------------------------------
### write variable values to file

  ncvar_put(tmmn_nc, "tmmn", tmmn, start = st, count = co)
  nc_sync(tmmn_nc) 

  ncvar_put(pr_nc, "pr", pr, start = st, count = co)
  nc_sync(pr_nc) 
  
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

