rm(list=ls(all=TRUE))

suppressMessages(library(rSOILWAT2, quiet = TRUE))

suppressMessages(library(rSW2data, quiet = TRUE))
suppressMessages(library(RSQLite, quietly = TRUE))
suppressMessages(library(DBI, quietly = TRUE))

suppressMessages(library(raster, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))

#suppressMessages(library(pbdMPI, quiet = TRUE))

#suppressMessages(library(pbdNCDF4, quiet = TRUE))
suppressMessages(library(RNetCDF, quiet = TRUE))
suppressMessages(library(ncdf4, quiet = TRUE))

# variables --------------------------------------------------------------------
isParallel <- FALSE # set to FALSE if you dont want to use pbdMPI to execute runs in parallel 
nRuns = 5 #is 30 for point based netCDF, but changed to 5 here for testing purposes (this is the number of simulations for each grid?? I think? )

# Begin ------------------------------------------------------------------------
file_list <- list.files(path = "./functions/", full.names = TRUE)

# Iterate over the file list and source each file. TO DO: package all these functions
for (file in file_list) {
  print(file)
  source(file)
}

################### ------------------------------------------------------------
# Part 0 - Setup
################### ------------------------------------------------------------

#### ---------------------- Initialize MPI  ------------------------------- ####
if(isParallel) {
  
  rank <- comm.rank() # processor's rank
  size <- comm.size() # total processors (i.e. equal to tasks in the SLURM scripts)
  comm.print(size)
  
  n.workers <- size - 1 # reserve one for other activities
}

#### -------------------   Set Inputs and Parameters   ------------------   ####

# Weather and sites ------------------------------------------------------------
weatherDB <- rSOILWAT2::dbW_setConnection(
  dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')
 
# this stuff is on the HPC... # Alice will download to her local computer later
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
#Sites <- Sites[!is.na(Sites$region2),]

sites <- dim(Sites)[1]

if(isParallel) {
  alljid <- get.jid(n = sites, method = "block", all = FALSE) 
  comm.print(alljid) 
} else {
  alljid <- sites
}

# Date Info --------------------------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

# NWS Anomalies ----------------------------------------------------------------
TempAnomsWhole <- data.table::fread('main/CurrentAnomalyTempData.csv')
PPTAnomsWhole <- data.table::fread('main/CurrentAnomalyPPTData.csv')

#Vegetation/Prod ---------------------------------------------------------------
# data from previous work that Daniel did...? 
AllProdInfo <- data.table::fread('./main/Data/SWRuns_InputData_prod_pnv_1991-2020_v11.csv')
d <- AllProdInfo[-1,]

# Establish date and time info --------------------------------------
# Establish current month and how the 'LEAD's relate to months
# A LEAD of 1 relates to the forecast beginning where the currmonth + 1
monthLeads <- makeMonthLeadRelationshipTable(TempAnomsWhole[1:12,], currMonth)
# column headers "lead1, lead2, lead3" don't correspond to the "name" of a lead, 
#but rather to the three leads that you'll need to get data from to average for a given month 

#### ---------------------------- Outputs  -------------------------------- ####
# creates empty netCDFs to be filled with the simulation runs 
source('./main/implementation/01.1_create-netcdfs.R') # TO DO: Make this a function / obviously change this path
if(!interactive() & isParallel) comm.print('netCDFs created')

## this isn't running... I think there is a netCDF example file that is located on the HPC??

################### ------------------------------------------------------------
# Simulation begins 
################### ------------------------------------------------------------
if(!interactive() & isParallel) comm.print('begin simulations')

# Run simulation --------------------------------------------------------------

for (j in alljid) { # TO DO: use "while" not "for"
  i <- j
  
  ################### ------------------------------------------------------------
  # Part 1 - Getting and formatting historical weather data 
  ################### ------------------------------------------------------------
## should double check... do we need to have a copy of the database on each core? 
  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'main/Data/dbWeatherData_WesternUS_gridMET_1979-2021.sqlite3')
  
  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  LatIdx <- Sites$LatIndex[i]
  LonIdx <- Sites$LonIndex[i]
  CDRegion <- Sites$region2[i]
  
  st <- c(LonIdx, LatIdx, 1)

  if(!interactive() & isParallel) comm.print(paste(i, ': Site', Site_id, 'running', Sys.time()))
  
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  wdata_plus <- getWeatherData(Lat, Long, currYear,
                                    dir = 'main/Data/www.northwestknowledge.net/metdata/data/')

  lastWeatherDate <- wdata_plus[[2]]
  wdata_plus <- wdata_plus[[1]]
  wdata_plus <- rSOILWAT2::dbW_weatherData_round(rSOILWAT2::dbW_dataframe_to_weatherData(
    wdata_plus[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]), digits= 4)
   
  clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, year.start = 1991, 
                                      year.end = 2020, do_C4vars = TRUE)
  wdata <- c(wdata, wdata_plus)
  
  ################### ----------------------------------------------------------
  # Part 2 - Sets SW parameters besides weather
  ################### ----------------------------------------------------------
  #setting soilWat specific options
  sw_in <- new("swInputData") # baseline data
  sw_in <- setVeg(sw_in, AllProdInfo, i)
  sw_in <- setSW(sw_in, Lat, Long, clim)
  #sw_in <- set_soils(sw_in, 2, 35, 35)
  sw_in@site@SoilTemperatureFlag <- FALSE # turns off the soil temp option 
  swCarbon_Use_Bio(sw_in) <- FALSE # turns off carbon
  swCarbon_Use_WUE(sw_in) <- FALSE # turns off Water use efficiency 
  swYears_EndYear(sw_in) <- currYear - 1 # the setting for the historical simulation 
  
  # Soils info formatting ----------------------------------------------------
  # waiting on the proper soils data, this is sort of a placeholder
  SoilsDF <- data.frame(depth_cm = c(1:250),
                        Depth = c(rep('Shallow', 15),
                                  rep('Intermediate', 50), #16 - 65
                                  rep('Deep',185))) # 66 - 250
  
  Soils <- data.frame(sw_in@soils@Layers)[,c('depth_cm', 'sand_frac', 'clay_frac')]
  Soils$width <- diff(c(0, Soils$depth_cm))
  SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
  SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])
  
  ##AES made these changes... not sure if they're okay? 
  # set "version" in sw_in?? AES did this... not sure if it is ok?
  sw_in@version <- "7.2.0"
  # set SWRC (soil water retention curve) for this run to default version?? AES 
  sw_in@site@swrc_flags <- c("swrc_name" = "Campbell1974", "ptf_name" = "Cosby1984AndOthers")
  sw_in@prod@veg_method <- as.integer(0)
  sw_in@estab@useEstab <- FALSE
  swSWC_use(sw_in) <- FALSE
  swSWC_Method(sw_in) <- as.integer(2)
  swSite_SoilDensityInputType(sw_in) <- as.integer(1)
   #sw_in@site@SoilDensityInputType <- test@site@SoilDensityInputType
  ################### ----------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------
  #if(!interactive()) comm.print(paste('Running Current Site', Site_id, Sys.time()))
  # runs SOILWAT2 for the historical data, and aggregates the results 
  # Note: hypothetically, we could run the historical period once, and then each 
  # month would run this part for the most recent month of historical data before running the anomaly data
  sw_example <- rSOILWAT2::sw_exampleData
  sw_out <- rSOILWAT2::sw_exec(inputData = sw_example, weatherList = wdata)

  ################ -------------------------------------------------------------
  # FORMAT OUTPUTS    --- Get 18 month median, 10, and 90 for all !!! ---
  ################ -------------------------------------------------------------
  
  HistDataAll <- getOutputs(sw_out, sw_in, SoilsDF, 
                            TimePeriod = 'Historical',
                            calc_EcoVars = TRUE)
  
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
  Hist_Shriver2018 <- data.table(Year = HistDataAll[[2]]$PlantedinYear,
                                 Prob = p_Shriver2018(HistDataAll[[2]]$Temp_mean, HistDataAll[[2]]$VWC_mean))

  Hist_GISSM <-  HistDataAll[[3]]
   
  # Hist_OConnor2020 <- HistDataAll[[4]]
  
  
  ################### ----------------------------------------------------------------
  # Part 4 - Run SOILWAT with future anomaly data!!
  ################### ----------------------------------------------------------------
  #if(!interactive()) comm.print(paste('Running Future Site', Site_id, Sys.time()))

  # 4.1 Climate Info from NWS ------------------------
  Nleads <- 12 # is an option to have it 13, but is much easier to do 12 months 
  # rather than a year and 1 month (plus the 13th month would be a bad forecast anyway)
  
  # in the TempAnom and PPTAnoms data, the Month listed as "current month" is included in the first lead 

  TempAnoms <- subset(TempAnomsWhole, CD == CDRegion)
  TempAnoms <- TempAnoms[1:Nleads]

  PPTAnoms <- subset(PPTAnomsWhole, CD == CDRegion)
  PPTAnoms <- PPTAnoms[1:Nleads,]

  # function in "weatherFunctions.R"
  AnomalyData1 <- runFutureSWwithAnomalies(sw_in0 = sw_in, wdata, SoilsDF,
                                           TempAnoms, PPTAnoms,
                                           Nleads, n = nRuns,
                                           currDOY, currMonth, currYear, currDate)
  #if(!interactive()) comm.print('done future')

  ################ -------------------------------------------------------------
  # FORMAT OUTPUTS  --- Get 18 month median, 10, and 90 for all !!! ---
  ################ -------------------------------------------------------------
  
  # 1 ------- Get means across all simulations for each run_year-year-day for all variables
  # AnomalyData3 <- plyr::aaply(plyr::laply(AnomalyData2, as.matrix), c(2,3), mean)

  # 2 - Get rolling means and then the quantiles of the rolling mean across sims --
  AnomalyData2 <- do.call(rbind, AnomalyData1[[1]])
  AnomRunStats <- formatOutputsFuture(AnomalyData2, SoilsDF, currDate)
  
  # 3 ------- Subset the Recent Past (6 months prior to current) from future ---
  AnomRunStats <- AnomRunStats[AnomRunStats$Date < lastWeatherDate, ]
  
  # # 4 ---------- Upcoming year (current date + 1 year)
  AnomalyData2 <- data.frame(AnomalyData2)
  AnomRunStats2 <- formatOutputsMonthlys(AnomalyData2, SoilsDF, 'future', currDate = currDate)

  ################### ----------------------------------------------------------
  # Part 5 - Calculate deltas, format outputs
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
  
  ################### ----------------------------------------------------------
  # Part 5.2 - Ecovar formatting
  ################### ----------------------------------------------------------
  
  # # Shriver Sagebrush
  Future_Shriver2018 <- dplyr::bind_rows(AnomalyData1[[2]], .id = "run")
             
  Future_Shriver2018 <- data.table(Year = Future_Shriver2018$PlantedinYear,
                                   run = Future_Shriver2018$run,
                                   Prob =  p_Shriver2018(Future_Shriver2018$Temp_mean, Future_Shriver2018$VWC_mean))
  
  Shriver_Stats <- formatShriver2018(Hist_Shriver2018, Future_Shriver2018, currYear)

  # GISSM Sagebrush
  Future_GISSM <- dplyr::bind_rows(AnomalyData1[[3]], .id = "run")
  Future_GISSM <- formatfutureGISSM(Future_GISSM)
  
  # TO DO: Need to discuss with group what this output is looks like as netCDf/map
  # Oconnor_Stats <- formatOConnor2020(Hist_OConnor2020, Future_OConnor2020) 
  
  ################### ----------------------------------------------------------
  # Part 6 - Insert into netCDFs!!!
  ################### ----------------------------------------------------------

  # TO DO: Another netCDF that tracks success and failure
  
  if(!interactive() & isParallel) comm.print('Inserting into netCDFs.', Sys.time())
  
  # TO DO: Make this into a function not a script
  source('./main/implementation/01.2_input-values-into-ncdfs.R') 
  
}


# Shut down MPI ---------------------------------------------------------------

if(!interactive() & isParallel) {
  
  #   #info.free()
  comm.print('done')
  rSOILWAT2::dbW_disconnectConnection()
  barrier()
  finalize()
}
