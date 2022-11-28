rm(list=ls(all=TRUE))
suppressMessages(library(rSOILWAT2, quiet = TRUE))
suppressMessages(library(rSW2data, quiet = TRUE))
suppressMessages(library(RSQLite, quietly = TRUE))
suppressMessages(library(DBI, quietly = TRUE))

suppressMessages(library(raster, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))
suppressMessages(library(zoo, quietly = TRUE))
suppressMessages(library(caTools, quietly = TRUE))

suppressMessages(library(pbdMPI, quiet = TRUE))
suppressMessages(library(RNetCDF, quiet = TRUE))

source('functions/weatherFunctions.R')
source('functions/HelperFunctions.R')
source('functions/Outputs.R')
source('main/implementation/SWfunc.R')
source('functions/soilsAndComp.R')
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
#source('projects/03-Setup-All-netCDFs/Create-template-netCDFs.R') # obviously change this path

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
for (i in alljid) { # use while not for
  
  
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
  
  st <- c(LatIdx, LonIdx, 1)
  co <- c(1, 1, 365)
  
  if(!interactive()) comm.print(paste('Site', Site_id, 'running'))
  
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  years <- rSOILWAT2::get_years_from_weatherData(wdata)
  ids <- rSOILWAT2:::select_years(years, 1990, 2021)
  wdata <- wdata[ids]
  
  clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, year.start = 1991, 
                                      year.end = 2020, do_C4vars = TRUE)

  ################### ----------------------------------------------------------
  # Part 2 - Sets SW parameters besides weather
  ################### ----------------------------------------------------------
  sw_in <- new("swInputData") # baseline data
  sw_in <- setVeg(sw_in, AllProdInfo, i)
  sw_in <- setSW(sw_in, Lat, Long, clim)
  #sw_in <- set_soils(sw_in, 2, 35, 35)
  sw_in@site@SoilTemperatureFlag <- FALSE
  swCarbon_Use_Bio(sw_in) <- FALSE
  swCarbon_Use_WUE(sw_in) <- FALSE
  swYears_EndYear(sw_in) <- currYear - 1
  
  ################### ----------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------
  if(!interactive()) comm.print(paste('Running Current Site', Site_id, Sys.time()))
  
  sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata, quiet = TRUE)
  
  
  ################### ----------------------------------------------------------
  # Part 4 - Gather Outputs
  ################### ----------------------------------------------------------
  
  # Soils info formatting ----------------------------------------------------
  SoilsDF <- data.frame(depth_cm = c(1:250),
                        Depth = c(rep('Shallow', 15),
                                  rep('Intermediate', 50), #16 - 65
                                  rep('Deep',185))) # 66 - 250
  
  Soils <- data.frame(sw_in@soils@Layers)[,c('depth_cm', 'sand_frac', 'clay_frac')]
  Soils$width <- diff(c(0, Soils$depth_cm))
  SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
  SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])
  
  ################ -------------------------------------------------------------
  # FORMAT OUTPUTS ------------- Get 18 month median, 10, and 90 for all !!! ---
  ################ -------------------------------------------------------------
  
  HistDataAll <- getOutputs(sw_out, sw_in, SoilsDF, 
                            TimePeriod = 'Historical',
                            calc_EcoVars = FALSE)
  
  # 1 - Get rolling mean -------------------------------------------------------
  HistDataAll1 <- setorder(as.data.frame(HistDataAll[[1]]), Year, Day)
  HistDataRolling <- getRolling(HistDataAll1)
  
  # 2 - Get climatologies! On the 18 month level!  -----------------------------
  
  # ---- Need to account for the 30-year baseline of the calc ...
  # ---- Each calc return 366 days. "get18MonthClimatologicalRecord" slices it all apart ...
  
  # previous 6 months
  HistData_Norm_Stats1 <- getHistoricalClimatology(HistDataRolling, 1991, 2021, SoilsDF)
  # this year
  HistData_Norm_Stats2 <- getHistoricalClimatology(HistDataRolling, 1991, 2020, SoilsDF)
  # next year
  HistData_Norm_Stats3 <- getHistoricalClimatology(HistDataRolling, 1992, 2021, SoilsDF)
  
  # 3 - Aggregate these chunks! ------------------------------------------------
  # Skip!! because as it stands this will change every month ......
  
  # HistDataNormMean_18MNs <- get18MonthClimatologicalRecord(HistData_Norm_Stats1,
  #                                                          HistData_Norm_Stats2,
  #                                                          HistData_Norm_Stats3,
  #                                                          currDate, currMonth,
  #                                                          currYear, todayMonthDay)
  
  # 4 - Additional outputs for "future" results --------------------------------
  # Questions where should I save these for easy loading when I need them .......
  
  HistData_MonthlyMeans_2 <- formatOutputs_Monthlys(HistDataAll1, SoilsDF, 
                                                    'historical', 1991, 2020, 
                                                    currDate, todayMonthDay,
                                                    currYearClimatology = TRUE)
  
  HistData_MonthlyMeans_3 <- formatOutputs_Monthlys(HistDataAll1, SoilsDF, 
                                                    'historical', 1992, 2021)
  
  # make one year record - skip because this will change!
  # HistData_MonthlyMeans <- formatHistoricalMonthlys(HistData_MonthlyMeans_2,
  #                                                   HistData_MonthlyMeans_3,
  #                                                   currYear, todayMonthDay)
  
  
  # eco vars!!!! ------------------------------------------------------------------
  # Hist_Shriver2018 <- data.table(Year = HistDataAll[[2]]$PlantedinYear,
  #                                Prob = p_Shriver2018(HistDataAll[[2]]$Temp_mean, HistDataAll[[2]]$VWC_mean))
  # 
  # Hist_GISSM <- data.table(HistDataAll[[3]])
  # 
  # Hist_OConnor2020 <- HistDataAll[[4]]
  
  
  
  ################### ----------------------------------------------------------
  # Part 4 - What if I just save everything as like a bajillion .csv at this point?
  ################### ----------------------------------------------------------
  
  Outpath = 'projects/04-WesternUs-Historical-SOILWAT-Run/Outputs'
  filestart = paste(Site_id, round(Lat, 5), round(Long, 5), sep = '_')
  
  write.csv(HistData_Norm_Stats1, 
            file.path(Outpath, 'DailyStats', paste(filestart, 'histNormDaily1.csv', sep = '_' )),
            row.names = FALSE)
  write.csv(HistData_Norm_Stats2,
            file.path(Outpath, 'DailyStats', paste(filestart, 'histNormDaily2.csv', sep = '_' )),
            row.names = FALSE)
  write.csv(HistData_Norm_Stats3,
            file.path(Outpath, 'DailyStats', paste(filestart, 'histNormDaily3.csv', sep = '_' )),
            row.names = FALSE)
  write.csv(HistData_MonthlyMeans_2,
            file.path(Outpath, 'MonthlyMeans', paste(filestart, 'histMonthlyMean1.csv', sep = '_' )),
            row.names = FALSE)
  write.csv(HistData_MonthlyMeans_3,
            file.path(Outpath, 'MonthlyMeans', paste(filestart, 'histMonthlyMean2.csv', sep = '_' )),
            row.names = FALSE)
  

}


# Shut down MPI ---------------------------------------------------------------

if(!interactive()) {
  
  #   #info.free()
  barrier()
  finalize()
}
  