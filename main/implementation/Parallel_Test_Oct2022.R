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
source('functions/getOutputs.R')
source('functions/formatOutputs.R')
source('functions/calcHistoricalClimatologies.R')

source('main/implementation/SWfunc.R')
source('functions/netcdf_functions_HPC.R')

################### ------------------------------------------------------------
# Part 0 - Setup
################### ------------------------------------------------------------

#### ---------------------- Initialize MPI  ------------------------------- ####
if(!interactive()) {
  rank <- comm.rank() # processor's rank
  size <- comm.size() # total processors (i.e. equal to tasks in the SLURM scripts)
  comm.print(size)
  
  n.workers <- size - 1 # reserve one for other activities
  
  #296006 sites
  alljid <- get.jid(n = 10000, method = "block", all = FALSE) 
  comm.print(alljid) 
}

#### ---------------------------- Outputs  -------------------------------- ####
source('projects/05-Setup-futureMonthly-netCDFs/Create-template-netCDFs.R') # obviously change this path
if(!interactive()) comm.print('netCDFs created')

#### --------------------   Set Inputs and Parameters   ------------------- ####

# Weather and sites ------------------------------------------------------------
weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
Sites <- Sites[!is.na(Sites$region2),]

# Date Info --------------------------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

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
if(!interactive()) comm.print('begin simulations')

# Run simulation --------------------------------------------------------------
for (j in alljid) { # use while not for
  
  #sites_for_now <- 15000:15050
  i <- j
  
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
  CDRegion <- Sites$region2[i]
  
  st <- c(LatIdx, LonIdx, 1)
  co <- c(1, 1, 365)

  if(!interactive()) comm.print(paste('Site', Site_id, 'running'))
  
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
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
  #sw_in <- set_soils(sw_in, 2, 35, 35)
  sw_in@site@SoilTemperatureFlag <- FALSE
  swCarbon_Use_Bio(sw_in) <- FALSE
  swCarbon_Use_WUE(sw_in) <- FALSE
  swYears_EndYear(sw_in) <- currYear - 1
  
  # Soils info formatting ----------------------------------------------------
  SoilsDF <- data.frame(depth_cm = c(1:250),
                        Depth = c(rep('Shallow', 15),
                                  rep('Intermediate', 50), #16 - 65
                                  rep('Deep',185))) # 66 - 250
  
  Soils <- data.frame(sw_in@soils@Layers)[,c('depth_cm', 'sand_frac', 'clay_frac')]
  Soils$width <- diff(c(0, Soils$depth_cm))
  SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
  SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])
  
  ################### ----------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------
  #if(!interactive()) comm.print(paste('Running Current Site', Site_id, Sys.time()))
  
  sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata, quiet = TRUE)
  
  ################ -------------------------------------------------------------
  # FORMAT OUTPUTS    --- Get 18 month median, 10, and 90 for all !!! ---
  ################ -------------------------------------------------------------
  
  HistDataAll <- getOutputs(sw_out, sw_in, SoilsDF, 
                            TimePeriod = 'Historical',
                            calc_EcoVars = FALSE)
  
  # 1 - Get rolling mean -------------------------------------------------------
  HistDataAll1 <- setorder(as.data.frame(HistDataAll[[1]]), Year, Day)
  HistDataRolling <- getRolling(HistDataAll1, TimePeriod = 'Historical')
  
  # 2 - Get climatologies! Daily for 18 months 549  -----------------------------
  
  # ---- Need to account for the 30-year baseline of the calc ...
  # ---- Each calc return 366 days. "get18MonthClimatologicalRecord" slices it all apart ...
  
  # previous 6 months
  HistData_Norm_Stats1 <- getHistoricalClimatology(HistDataRolling, 1991, 2021, SoilsDF)
  # this year
  HistData_Norm_Stats2 <- getHistoricalClimatology(HistDataRolling, 1991, 2020, SoilsDF)
  # next year
  HistData_Norm_Stats3 <- getHistoricalClimatology(HistDataRolling, 1992, 2021, SoilsDF)
  
  # 3 - Aggregate these chunks! ------------------------------------------------
  HistDataNormMean_18MNs <- get18MonthClimatologicalRecord(HistData_Norm_Stats1,
                                                           HistData_Norm_Stats2,
                                                           HistData_Norm_Stats3,
                                                           currDate, currMonth,
                                                           currYear, todayMonthDay)
  
  # 4 - Additional outputs for "future" results --------------------------------
  # Questions where should I save these for easy loading when I need them .......
  HistData_MonthlyMeans_2 <- formatOutputsMonthlys(HistDataAll1, SoilsDF, 
                                                    'historical', 1991, 2020, 
                                                    currDate, todayMonthDay,
                                                    currYearClimatology = TRUE)
  
  HistData_MonthlyMeans_3 <- formatOutputsMonthlys(HistDataAll1, SoilsDF, 
                                                    'historical', 1992, 2021)
  
  # make one year record -
  HistData_MonthlyMeans <- formatHistoricalMonthlys(HistData_MonthlyMeans_2,
                                                   HistData_MonthlyMeans_3,
                                                   currYear, todayMonthDay)
  
  # eco vars!!!! ------------------------------------------------------------------
  # Hist_Shriver2018 <- data.table(Year = HistDataAll[[2]]$PlantedinYear,
  #                                Prob = p_Shriver2018(HistDataAll[[2]]$Temp_mean, HistDataAll[[2]]$VWC_mean))
  # 
  # Hist_GISSM <- data.table(HistDataAll[[3]])
  # 
  # Hist_OConnor2020 <- HistDataAll[[4]]
  
  
  ################### ----------------------------------------------------------------
  # Part 4 - Run SOILWAT with future anomaly data!!
  ################### ----------------------------------------------------------------
  if(!interactive()) comm.print(paste('Running Future Site', Site_id, Sys.time()))

  # 4.1 Climate Info from NWS ------------------------
  Nleads <- 12

  TempAnoms <- subset(TempAnomsWhole, CD == CDRegion)
  TempAnoms <- TempAnoms[1:Nleads]

  PPTAnoms <- subset(PPTAnomsWhole, CD == CDRegion)
  PPTAnoms <- PPTAnoms[1:Nleads,]

  AnomalyData1 <- runFutureSWwithAnomalies(sw_in0 = sw_in, wdata, SoilsDF,
                                           TempAnoms, PPTAnoms,
                                           Nleads, n = 5,
                                           currDOY, currMonth, currYear, currDate)
  if(!interactive()) comm.print('done future')

  ################ -------------------------------------------------------------
  # FORMAT OUTPUTS  --- Get 18 month median, 10, and 90 for all !!! ---
  ################ -------------------------------------------------------------
  
  # 1 ------- Get means across all simulations for each run_year-year-day for all variables
  # AnomalyData3 <- plyr::aaply(plyr::laply(AnomalyData2, as.matrix), c(2,3), mean)

  # 2 - Get rolling means and then the quantiles of the rolling mean across sims --
  AnomalyData2 <- do.call(rbind, AnomalyData1)
  AnomRunStats <- formatOutputsFuture(AnomalyData2, SoilsDF, currDate)
  
  # 3 ------- Subset the Recent Past (6 months prior to current) from future ---
  AnomRunStats <- AnomRunStats[AnomRunStats$Date < lastWeatherDate, ]
  
  # # 4 ---------- Upcoming year (current date + 1 year)
  AnomalyData2 <- data.frame(AnomalyData2)
  AnomRunStats2 <- formatOutputsMonthlys(AnomalyData2, SoilsDF, 'future', currDate = currDate)

  
  ################### ----------------------------------------------------------
  # Part 5 - Calculate deltas, formout outputs
  ################### ----------------------------------------------------------
  
  # calculate deltas and approx and format
  Vars <- names(HistDataAll1)[3:dim(HistDataAll1)[2]]
  if(length(Vars) == 3) Vars <- c(Vars, "SWP.Shallow")
  if(length(Vars) == 4) Vars <- c(Vars, "SWP.Shallow", "SWP.Intermediate")
  if(length(Vars) == 5) Vars <- c(Vars, "SWP.Shallow", "SWP.Intermediate", "SWP.Deep")
  
  AllVarData <- data.frame(Date = seq((currDate-183), (currDate+365), "days"))
  
  for(v in seq(Vars)){
    OneVarData <- suppressMessages(calcDeltasApproxAndFormat(HistData_Norm_Stats1, HistData_MonthlyMeans,
                                                             as.data.frame(HistDataNormMean_18MNs),
                                                             AnomRunStats, AnomRunStats2,
                                                             Vars[v], currDate, todayMonthDay, currYear,
                                                             lastWeatherDate))
    
    AllVarData <- merge(AllVarData, OneVarData)
  }
  
  
  # eco vars ------------------------------------------------------------------
  # Future_Shriver2018 <- data.table(Year = AnomalyData1[[2]]$PlantedinYear, 
  #                                  run = AnomalyData1[[2]]$run,
  #                                  Prob =  p_Shriver2018(AnomalyData1[[2]]$Temp_mean, AnomalyData1[[2]]$VWC_mean))
  # 
  # Future_GISSM <- data.table(AnomalyData1[[3]])
  # 
  # Future_OConnor2020 <- data.table(AnomalyData1[[4]])
  
  # format ecovars for writing out -------------------------------------------
  # Shriver_Stats <- formatShriver2018(Hist_Shriver2018, Future_Shriver2018, currYear)
  # GISSM_Stats <- formatGISSM(Hist_GISSM, Future_GISSM)
  # Oconnor_Stats <- formatOConnor2020(Hist_OConnor2020, Future_OConnor2020)
  
  ################### ----------------------------------------------------------
  # Part 6 - Insert into netCDFs!!!
  ################### ----------------------------------------------------------

  
  
  # Another netCDF that tracks success and failure
  
  
  
}


# Shut down MPI ---------------------------------------------------------------

if(!interactive()) {
  
  #   #info.free()
  comm.print('done')
  rSOILWAT2::dbW_disconnectConnection()
  barrier()
  finalize()
}
