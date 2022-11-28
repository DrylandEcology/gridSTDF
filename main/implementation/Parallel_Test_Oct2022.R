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
source('main/implementation/SWfunc.R')
source('functions/netcdf_functions2.R')

################### ------------------------------------------------------------
# Part 0 - Setup
################### ------------------------------------------------------------

#### ---------------------- Initialize MPI  ------------------------------- ####
rank <- comm.rank() # processor's rank
size <- comm.size() # total processors (i.e. equal to tasks in the SLURM scripts)
comm.print(size)

n.workers <- size - 1 # reserve one for other activities

#296006 sites
alljid <- get.jid(n = 1000, method = "block", all = FALSE) 
comm.print(alljid)

#### ---------------------------- Outputs  -------------------------------- ####
source('projects/04-Setup-All-netCDFs/Create-template-netCDFs.R') # obviously change this path

#### --------------------   Set Inputs and Parameters   ------------------- ####

# Weather and sites ------------------------------------------------------------
weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')
#Sites <- rSOILWAT2::dbW_getSiteTable()
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))

# Date Info --------------------------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

# CD Region Shapefile ----------------------------------------------------------
CD102 <- shapefile(x = 'main/CD102/CD102.shp')

# NWS Anomalies ----------------------------------------------------------------
TempAnomsWhole <- data.table::fread('main/CurrentAnomalyTempData.csv')
PPTAnomsWhole <- data.table::fread('main/CurrentAnomalyPPTData.csv')

#Vegetation/Prod ----------------------------------------------------------------
AllProdInfo <- data.table::fread('main/Data/SWRuns_InputData_prod_pnv_1991-2020_v11.csv')
AllProdInfo <- AllProdInfo[-1,]

# Establish date and time info --------------------------------------
# Establish current month and how the 'LEAD's relate to months
# A LEAD of 1 relates to the forecast beginning where the currmonth + 1
monthLeads <- makeMonthLeadRelationshipTable(TempAnomsWhole[1:12,], currMonth)

################### ------------------------------------------------------------
# Simulation begin in Parallel!! 
################### ------------------------------------------------------------

# Run simulation --------------------------------------------------------------
for (j in alljid) { # use while not for
  
  sites_for_now <- 10000:10050
  i <- sites_for_now[j]
  ################### ------------------------------------------------------------
  # Part 1 - Getting and formatting historical weather data 
  ################### ------------------------------------------------------------

  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3')
  
  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  LatIdx <- Sites$LatIndex[i]
  LonIdx <- Sites$LonIndex[i]
  
  st <- c(LatIdx, LonIdx, 1)
  co <- c(1, 1, 365)

  if(!interactive()) comm.print(paste('Site', Site_id, 'running'))
  
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  #years <- rSOILWAT2::get_years_from_weatherData(wdata)
  #ids <- rSOILWAT2:::select_years(years, 1990, 2021)
  #wdata <- wdata[ids]

  wdata_plus <- getWeatherData(Lat, Long, currYear,
                                    dir = 'main/Data/www.northwestknowledge.net/metdata/data/')

  lastWeatherDate <- wdata_plus[[2]]
  wdata_plus <- wdata_plus[[1]]
  wdata_plus <- rSOILWAT2::dbW_dataframe_to_weatherData(
    wdata_plus[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
   
  clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, year.start = 1991, 
                                      year.end = 2020, do_C4vars = TRUE)
  wdata <- c(wdata, wdata_plus)
  
  ################### ----------------------------------------------------------
  # Part 2 - Sets SW parameters besides weather
  ################### ----------------------------------------------------------
  sw_in <- new("swInputData") # baseline data
  sw_in <- setVeg(sw_in, AllProdInfo, i)
  sw_in <- setSW(sw_in, Lat, Long, clim)
  sw_in@site@SoilTemperatureFlag <- FALSE
  
  ################### ----------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------
  #if(!interactive()) comm.print(paste('Running Current Site', Site_id, Sys.time()))
  
  #sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata, quiet = FALSE)
  
  ################### ----------------------------------------------------------------
  # Part 4 - Run SOILWAT with future anomaly data!!
  ################### ----------------------------------------------------------------
  if(!interactive()) comm.print(paste('Running Future Site', Site_id, Sys.time()))
  #print(paste('Running Future', Sys.time()))
  
  # 4.1 Determine Region from coordinates and shapefile ------------------------
  points <- data.frame(x = swSite_IntrinsicSiteParams(sw_in)[1], 
                       y = swSite_IntrinsicSiteParams(sw_in)[2])
  coordinates(points) <- ~ x + y
  proj4string(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  CDRegion <- as.numeric(over(points, CD102)$ID4)
  
  # subset anomaly data --------------------------------------------------------
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
                                           Nleads, n = 10,
                                           currDOY, currMonth, currYear, currDate)
  
  AnomalyData1 <- plyr::aaply(plyr::laply(AnomalyData1, as.matrix), c(2, 3), mean)
  
  
  # Put data into netCDFs------------------------------------------------------
  wdata_2022 <- wdata[['2022']]
  wdata_2022_tmax <- as.vector(wdata_2022@data[,2])
  
  ### write variable values to file
  
  ncvar_put(tmmx_nc, "tmmx", wdata_2022_tmax, start = st, count = co)
  nc_sync(tmmx_nc) 
  # Another netCDF that tracks success and failure
  
}


# Shut down MPI ---------------------------------------------------------------

if(!interactive()) {
  
  #   #info.free()
  barrier()
  finalize()
}