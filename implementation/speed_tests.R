rm(list=ls(all=TRUE))
library(rSOILWAT2)
library(rSW2data)
library(DBI)
library(RSQLite)
library(raster)
library(data.table)
library(lubridate)

library(foreach)
library(doParallel)


library(tictoc)
library(profvis)
#####---------------------------------------------------------------------------
#Prep
source('functions/weatherFunctions.R')
source('functions/HelperFunctions.R')
source('functions/Outputs.R')
source('SWfunc.R')

print(paste('Begin', Sys.time()))
tic('Begin')

con <- DBI::dbDriver('SQLite')
weatherDB <- dbConnect(con, 
                       "Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3")
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

# Establish date and time info --------------------------------------
# Establish current month and how the 'LEAD's relate to months
# A LEAD of 1 relates to the forecast beginning where the currmonth + 1
monthLeads <- data.table(makeMonthLeadRelationshipTable(TempAnomsWhole[1:12,], currMonth))

sql <- "SELECT Latitude, Longitude FROM Sites WHERE Site_id = :x1"
res <- DBI::dbGetQuery(weatherDB, sql, params = list(x1 = 10000))
Lat <- unlist(res[1])
Long <- unlist(res[2])

sql <- "SELECT data FROM WeatherData WHERE Site_id = :x1 AND Scenario = 1"
res <- DBI::dbGetQuery(weatherDB, sql, params = list(x1 = 10000))[1, 1]

if (is.na(res) || all(lengths(res) == 0)) {
  stop(paste("Weather data for site", shQuote(IDs[["site_id"]]), 
             "and scenario", shQuote(IDs[["scenario_id"]]), 
             "does not exist in weather database."))
}

wdata <- try(dbW_blob_to_weatherData(res))
years <- rSOILWAT2::get_years_from_weatherData(wdata)
ids <- rSOILWAT2:::select_years(years, 1990, 2020)
wdata <- wdata[ids]

#if (inherits(wdata, "try-error")) {
#  stop(paste("Weather data for site", shQuote(IDs[["site_id"]]), 
#             "and scenario", shQuote(IDs[["scenario_id"]]), "is corrupted."))
#}

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

Nleads = 12
n = 10
sw_in0 = sw_in

SoilsDF <- data.table(depth_cm = c(1:250),
                      Depth = c(rep('Shallow', 15), 
                                rep('Intermediate', 50), 
                                rep('Deep',185)))

Soils <- data.table((sw_in@soils@Layers))[,c('depth_cm', 'sand_frac', 'clay_frac')]
Soils$width <- diff(c(0, as.numeric(unlist(Soils[,'depth_cm']))))
SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])

AllOut1 <- list()

tic('MainFunc')
AnomalyData1 <- runFutureSWwithAnomalies(sw_in0 = sw_in, wdata, SoilsDF,
                                         TempAnoms, PPTAnoms, 
                                         Nleads, n = 10, 
                                         currDOY, currMonth, currYear, currDate)

toc()


    # profvis(getOutputs(sw_out, sw_in0, calc_EcoVars = FALSE, TimePeriod = 'Future',
    #            currYear, currDate))
    # 
    #names(Out1[1]) <-  paste(nn, y, sep = '_')
    #AllOut1 <- c(AllOut1, Out1)
    
    # Shriver_Out <- rbind(Shriver_Out, cbind(Out1[[2]], run = paste(nn, y, sep = '_')))
    # GISSM_Out <- rbind(GISSM_Out, cbind(Out1[[3]], run = paste(nn, y, sep = '_')))
    # OConnor_Out <- rbind(OConnor_Out, Out1[[4]])


print(paste('End', Sys.time()))
toc()
gc()
