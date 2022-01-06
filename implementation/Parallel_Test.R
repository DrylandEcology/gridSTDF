rm(list=ls(all=TRUE))
library(rSOILWAT2)
library(rSW2data)
library(DBI)
library(RPostgres)
library(RSQLite)
library(raster)
library(data.table)
library(lubridate)
library(foreach)
library(doParallel)

#setwd('/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab')
#####---------------------------------------------------------------------------
#Prep
source('gridSTDF/functions/weatherFunctions.R')
source('gridSTDF/functions/HelperFunctions.R')
source('gridSTDF/functions/Outputs.R')
source('gridSTDF/implementation/SWfunc.R')
source('gridSTDF/implementation/sql_funcs.R')


numCores <- detectCores() - 1
cl <- makeCluster(numCores) # default is PSOCK cluster

# Defining packages and variables for cluster
clusterEvalQ(cl, {
  library(rSOILWAT2)
  library(rSW2data)
  library(DBI)
  library(RSQLite)
  library(raster)
  library(data.table)
  library(lubridate)
  library(plyr)
  library(stringr)
  
  db_user = 'mynonsuperuser'
  db_password = 'grid-stdf-2022'
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = 'gridSTDF_2022', 
                        user=db_user, password=db_password)
  
  con2 <- DBI::dbDriver('SQLite')
  weatherDB <- dbConnect(con2, 
                         "gridSTDF/Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3")
  NULL
})

registerDoParallel(cl)

################### ----------------------------------------------------------------
# Part 0 - Setup
################### ----------------------------------------------------------------

# Misc Info --------------------------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

# Inputs -----------------------------------------------------------------------

# CD Region Shapefile
CD102 <- shapefile(x = 'gridSTDF/CD102/CD102.shp')

# NWS Anomalies
TempAnomsWhole <- data.table::fread('gridSTDF/CurrentAnomalyTempData.csv')
PPTAnomsWhole <- data.table::fread('gridSTDF/CurrentAnomalyPPTData.csv')

# Establish date and time info --------------------------------------
# Establish current month and how the 'LEAD's relate to months
# A LEAD of 1 relates to the forecast beginning where the currmonth + 1
monthLeads <- makeMonthLeadRelationshipTable(TempAnomsWhole[1:12,], currMonth)

toEXPORT <- c('currDOY', 'currMonth', 'currYear', 'currDate', 'todayMonthDay', 
              'CD102', 'TempAnomsWhole', 'PPTAnomsWhole', 'monthLeads')

################### ------------------------------------------------------------
# Part 1 - Getting and formatting historical weather data 
################### ------------------------------------------------------------
print(paste('Begin Parallel', Sys.time()))

indexes = c(10000:10500)
gridSTDF_test_res <- foreach(i = indexes, 
                                .inorder=FALSE,
                                .combine='rbind',
                                .export = toEXPORT) %dopar% {
                                  
  
 # print(paste('Formatting Weather Data', Sys.time()))
  
  sql <- "SELECT Latitude, Longitude FROM Sites WHERE Site_id = :x1"
  res <- DBI::dbGetQuery(weatherDB, sql, params = list(x1 = i))
  Lat <- unlist(res[1])
  Long <- unlist(res[2])
  
  sql <- "SELECT data FROM WeatherData WHERE Site_id = :x1 AND Scenario = 1"
  res <- DBI::dbGetQuery(weatherDB, sql, params = list(x1 = i))[1, 1]
  
  if (is.na(res) || all(lengths(res) == 0)) {
    stop(paste("Weather data for site", shQuote(IDs[["site_id"]]), 
               "and scenario", shQuote(IDs[["scenario_id"]]), 
               "does not exist in weather database."))
  }
  
  wdata <- try(dbW_blob_to_weatherData(res))
  years <- rSOILWAT2::get_years_from_weatherData(wdata)
  ids <- rSOILWAT2:::select_years(years, 1990, 2020)
  wdata <- wdata[ids]

  
  wdata_2021_plus <- getWeatherData(Lat, Long, currYear,
                          dir = 'gridSTDF/Data/www.northwestknowledge.net/metdata/data/')
  
  lastWeatherDate <- wdata_2021_plus[[2]]
  wdata_2021_plus <- wdata_2021_plus[[1]]
  wdata_2021_plus <- rSOILWAT2::dbW_dataframe_to_weatherData(wdata_2021_plus[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
  
  clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
  wdata <- c(wdata, wdata_2021_plus)
  
  ################### ----------------------------------------------------------------
  # Part 2 - Sets SW parameters besides weather
  ################### ----------------------------------------------------------------
  sw_in <- setSW(Lat, Long)
  
  ################### ----------------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------------
  print(paste('Running Current', Sys.time()))
  sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
  
  ################### ----------------------------------------------------------------
  # Part 4 - Run SOILWAT with future anomaly data
  ################### ----------------------------------------------------------------
  print(paste('Running Future', Sys.time()))
  
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
  
  #debug(runFutureSWwithAnomalies)
  #debug(makeWeathOneSim)
  #print(paste('Begin', Sys.time()))
  AnomalyData1 <- runFutureSWwithAnomalies(sw_in0 = sw_in, wdata, SoilsDF,
                                           TempAnoms, PPTAnoms, 
                                           Nleads, n = 10, 
                                           currDOY, currMonth, currYear, currDate)
  
  AnomalyData1 <- plyr::aaply(plyr::laply(AnomalyData1, as.matrix), c(2, 3), mean)
  AnomalyData1 <- as.data.table(AnomalyData1)
  # Future monthly
  # # #  Save monthly mean and 10th and 90th percentiles
  # historical - last 6 months daily, separate table
  # ecological metrics
  
  ###### SQL functions
  inject_data(AnomalyData1)
  
  
  #print(paste('Done', Sys.time()))
        }


#clusterEvalQ(cl, {
#  RSQLite::dbDisconnect(con)
#})

stopCluster(cl)
stopImplicitCluster()

print(paste('End Parallel', Sys.time()))
gc()

#str(gridSTDF_test_res)
#View(gridSTDF_test_res[11])










