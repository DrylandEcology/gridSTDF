rm(list=ls(all=TRUE))
suppressMessages(library(rSOILWAT2, quiet = TRUE))
suppressMessages(library(rSW2data, quiet = TRUE))
suppressMessages(library(RSQLite, quietly = TRUE))
suppressMessages(library(DBI, quietly = TRUE))

suppressMessages(library(raster, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))

suppressMessages(library(pbdMPI, quiet = TRUE))
suppressMessages(library(RNetCDF, quiet = TRUE))


source('functions/weatherFunctions.R')
source('functions/HelperFunctions.R')
source('functions/Outputs.R')
source('implementation/SWfunc.R')
source('implementation/sql_funcs.R')

################### ----------------------------------------------------------------
# Part 0 - Setup
################### ----------------------------------------------------------------

# MPI Parameters ---------------------------------------------------------------
init()

rank <- comm.rank()
size <- comm.size()
hostname <- spmd.get.processor.name()
n.workers <- size - 1
info.create()


# Outputs  ------------------------------------------------------------
### Define global dimensions and data
# source(create_gridSTDF_ncdfs.R)

numRows <- 1386 # nrows
numCols <- 585 #ncols
days <- 1095 # 3 years worth of days!

# filename <- "test2.nc" 
# 
# ncid <- create.nc(filename, format="netcdf4", clobber = TRUE)#, mpi_comm=comm.c2f(), mpi_info=info.c2f())
# rdim <- dim.def.nc(ncid, "rows", numRows)
# cdim <- dim.def.nc(ncid, "cols", numCols)
# tdim <- dim.def.nc(ncid, "time", days)
# varid <- var.def.nc(ncid, "VWC", "NC_FLOAT", c(rdim, cdim, tdim))

# Sites ----------------------------------------------------------------------
weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3')
Sites <- rSOILWAT2::dbW_getSiteTable()

# Misc Info --------------------------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

# Inputs -----------------------------------------------------------------------

# CD Region Shapefile
CD102 <- shapefile(x = 'CD102/CD102.shp')

# NWS Anomalies
TempAnomsWhole <- data.table::fread('CurrentAnomalyTempData.csv')
PPTAnomsWhole <- data.table::fread('CurrentAnomalyPPTData.csv')

#Vegetation/Prod
AllProdInfo <- data.table::fread('Data/SWRuns_InputData_prod_pnv_1991-2020_v11.csv')
AllProdInfo <- AllProdInfo[-1,]

# Establish date and time info --------------------------------------
# Establish current month and how the 'LEAD's relate to months
# A LEAD of 1 relates to the forecast beginning where the currmonth + 1
monthLeads <- makeMonthLeadRelationshipTable(TempAnomsWhole[1:12,], currMonth)

################### ----------------------------------------------------------------
# Part 1 - Parallel!! 
################### ----------------------------------------------------------------

alljid <- get.jid(n = 4, method = "block", all = TRUE) 
comm.print(alljid)


# Run simulation --------------------------------------------------------------
for (j in alljid) { # use while not for
  
  sites_for_now <- 10000:10050
  i <- sites_for_now[j]
  ################### ------------------------------------------------------------
  # Part 1 - Getting and formatting historical weather data 
  ################### ------------------------------------------------------------

  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3')
  
  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  
  comm.print(paste('Site', Site_id, 'running'))
  
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  years <- rSOILWAT2::get_years_from_weatherData(wdata)
  ids <- rSOILWAT2:::select_years(years, 1990, 2020)
  wdata <- wdata[ids]

  wdata_2021_plus <- getWeatherData(Lat, Long, currYear,
                                    dir = 'Data/www.northwestknowledge.net/metdata/data/')

  lastWeatherDate <- wdata_2021_plus[[2]]
  wdata_2021_plus <- wdata_2021_plus[[1]]
  wdata_2021_plus <- rSOILWAT2::dbW_dataframe_to_weatherData(wdata_2021_plus[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
   
  clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
  wdata <- c(wdata, wdata_2021_plus)
  
  ################### ----------------------------------------------------------------
  # Part 2 - Sets SW parameters besides weather
  ################### ----------------------------------------------------------------
  sw_in <- new("swInputData") # baseline data
  sw_in <- setVeg(sw_in, AllProdInfo, i)
  sw_in <- setSW(sw_in, Lat, Long, clim)
  sw_in@site@SoilTemperatureFlag <- FALSE
  
  ################### ----------------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------------
  comm.print(paste(' Running Current Site', Site_id, Sys.time()))
  
  sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata, quiet = FALSE)
  
  ################### ----------------------------------------------------------------
  # Part 4 - Run SOILWAT with future anomaly data!!
  ################### ----------------------------------------------------------------
  comm.print(paste(' Running Future Site', Site_id, Sys.time()))
  #print(paste('Running Future', Sys.time()))
  
  # Determine Region from coordinates and shapefile ------------------------------
  points <- data.frame(x = swSite_IntrinsicSiteParams(sw_in)[1], 
                       y = swSite_IntrinsicSiteParams(sw_in)[2])
  coordinates(points) <- ~ x + y
  proj4string(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  CDRegion <- as.numeric(over(points, CD102)$ID4)
  
  # subset anomaly data -----------------------------------------------------------
  Nleads <- 12

  TempAnoms <- subset(TempAnomsWhole, CD == CDRegion)
  TempAnoms <- TempAnoms[1:Nleads]

  PPTAnoms <- subset(PPTAnomsWhole, CD == CDRegion)
  PPTAnoms <- PPTAnoms[1:Nleads,]

  SoilsDF <- data.table(depth_cm = c(1:250),
                        Depth = c(rep('Shallow', 15),
                                  rep('Intermediate', 50),
                                  rep('Deep',185)))

  Soils <- data.table((sw_in@soils@Layers))[,c('depth_cm', 'sand_frac', 'clay_frac')]
  Soils$width <- diff(c(0, as.numeric(unlist(Soils[,'depth_cm']))))
  SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
  SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])

  AnomalyData1 <- runFutureSWwithAnomalies(sw_in0 = sw_in, wdata, SoilsDF,
                                           TempAnoms, PPTAnoms,
                                           Nleads, n = 5,
                                           currDOY, currMonth, currYear, currDate)
  
  AnomalyData1 <- plyr::aaply(plyr::laply(AnomalyData1, as.matrix), c(2, 3), mean)
  
  
  # Put data into netCDFsv------------------------------------------------------
  
  # var.put.nc(ncfile = ncid, 
  #            variable = varid, 
  #            data  = AnomalyData1[,'VWC.Shallow'], # the data
  #            start=c(1, 1, 1), 
  #            count=c(1, 1, dazqys)
  # )
  
  
}


# Shut down MPI ---------------------------------------------------------------
close.nc(ncid)
info.free()
barrier()
finalize()