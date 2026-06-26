rm(list=ls(all=TRUE))

# install rNETCDF 
# R CMD INSTALL --configure-args="CPPFLAGS=-I/sw/include \
#     LDFLAGS=-L/sw/lib LIBS=-lhdf5 --with-mpicc=mpicc --with-mpiexec=mpiexec" \
# RNetCDF_2.9-1.tar.gz

# remotes::install_github("DrylandEcology/rSW2st")
# remotes::install_github("DrylandEcology/rSOILWAT2", build_vignettes = FALSE)
# remotes::install_github("DrylandEcology/rSW2funs")
 
# where R packages are located on the HPC

#.libPaths("/home/astears/R/x86_64-redhat-linux-gnu-library/4.2")


suppressMessages(library(rSOILWAT2, quiet = TRUE))

suppressMessages(library(rSW2data, quiet = TRUE))
suppressMessages(library(RSQLite, quietly = TRUE))
suppressMessages(library(DBI, quietly = TRUE))
suppressMessages(library(rSW2st, quietly = TRUE))
suppressMessages(library(rSW2funs, quietly = TRUE))
#suppressMessages(library(raster, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))

suppressMessages(library(pbdMPI, quiet = TRUE))

#suppressMessages(library(pbdNCDF4, quiet = TRUE))
suppressMessages(library(RNetCDF, quiet = TRUE))
#suppressMessages(library(ncdf4, quiet = TRUE))


# Read in sagebrush biome outline --------------------------------------------
whichSites <- read.csv("./main/Data/SagebrushBiomeList.csv")



# variables --------------------------------------------------------------------
isParallel <- FALSE # set to FALSE if you dont want to use pbdMPI to execute runs in parallel 
nRuns = 30 #is 30 for point based netCDF, but changed to 5 here for testing purposes (this is the number of simulations for each grid)

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
  dbFilePath = 'main/Data/dbWeatherData_WesternUS4km_gridMET_v20240509.sqlite3')
 
# this stuff is on the HPC... # Alice will download to her local computer later
#Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
#Sites <- Sites[!is.na(Sites$region2),]
# for version just w/ sagebrush biome 
Sites <- whichSites 

sites <- dim(Sites)[1]

# load gridded soils data from Daniel (currently an old version, will be updated w/ SOLUS100 data)
soils_gridClay <- RNetCDF::open.nc(con = "./main/Data/soilsDB_new/claytotal_PED-CONUS4km_SOLUS100.nc") 
soils_gridSand <- RNetCDF::open.nc(con = "./main/Data/soilsDB_new/sandtotal_PED-CONUS4km_SOLUS100.nc") 
soils_gridSilt <- RNetCDF::open.nc(con = "./main/Data/soilsDB_new/silttotal_PED-CONUS4km_SOLUS100.nc") 
soils_gridDensity <- RNetCDF::open.nc(con = "./main/Data/soilsDB_new/dbovendry_PED-CONUS4km_SOLUS100.nc") 
soils_gridThickness <- RNetCDF::open.nc(con = "./main/Data/soilsDB_new/hzthk_PED-CONUS4km_SOLUS100.nc") 
soils_gridCoarse <- RNetCDF::open.nc(con = "./main/Data/soilsDB_new/fragvol_PED-CONUS4km_SOLUS100.nc")
soilGridLats <- var.get.nc(soils_gridClay, "latitude")
soilGridLons <- var.get.nc(soils_gridClay, "longitude")

# if(isParallel) {
#   alljid <- get.jid(n = sites, method = "block", all = FALSE) 
#   comm.print(alljid) 
# } else {
#   alljid <- sites
# }

# for version just w/ sagebrush biome 
alljid <- sites


# Date Info --------------------------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- #format(Sys.Date() , format="%m-%d")
todayMonthDat <- ("02-10")

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
suppressWarnings(source('./main/implementation/01.1_create-netcdfs.R')) # TO DO: Make this a function / obviously change this path
if(!interactive() & isParallel) comm.print('netCDFs created')

################### ------------------------------------------------------------
# Simulation begins 
################### ------------------------------------------------------------
if(!interactive() & isParallel) comm.print('begin simulations')

# Run simulation --------------------------------------------------------------

for (j in 1242:alljid) { # TO DO: use "while" not "for"
  i <- j
 
  print(i)
  ################### ------------------------------------------------------------
  # Part 1 - Getting and formatting historical weather data 
  ################### ------------------------------------------------------------
## should double check... do we need to have a copy of the database on each core? 
  weatherDB <- rSOILWAT2::dbW_setConnection(
    dbFilePath = 'main/Data/dbWeatherData_WesternUS4km_gridMET_v20240509.sqlite3')
  
  Site_id <- Sites$Site_id[i]
  Lat <- Sites$Latitude[i]
  Long <- Sites$Longitude[i]
  LatIdx <- Sites$LatIndex[i]
  LonIdx <- Sites$LonIndex[i]
  CDRegion <- Sites$region2[i]
  
  st <- c(LonIdx, LatIdx, 1)

  if(!interactive() & isParallel) comm.print(paste(i, ': Site', Site_id, 'running', Sys.time()))
  
  wdata <- rSOILWAT2::dbW_getWeatherData(Site_id = Site_id)
  wdata_plus <- suppressWarnings(getWeatherData(Lat, Long, currYear,
                                    dir = 'main/Data/www.northwestknowledge.net/metdata/data/'))

  lastWeatherDate <- wdata_plus[[2]]
  wdata_plus <- wdata_plus[[1]]
  wdata_plus <-rSOILWAT2::dbW_dataframe_to_weatherData(
    wdata_plus[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')])
   
`clim` <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, year.start = 1991, 
                                      year.end = 2020, do_C4vars = TRUE)
  wdata <- c(wdata, wdata_plus)
  
  # hack to deal with issues in input data where min temp is slightly (usually by ~.1 degrees) above max temp
  for (k in 1:length(wdata)) {
    if (sum((wdata[[names(wdata)[k]]]@data[, "Tmax_C"] < wdata[[names(wdata)[k]]]@data[, "Tmin_C"])) > 0) {
      wdata[[names(wdata)[k]]]@data[wdata[[names(wdata)[k]]]@data[, "Tmax_C"] < wdata[[names(wdata)[k]]]@data[, "Tmin_C"], "Tmin_C"] <-
        wdata[[names(wdata)[k]]]@data[wdata[[names(wdata)[k]]]@data[, "Tmax_C"] < wdata[[names(wdata)[k]]]@data[, "Tmin_C"], "Tmax_C"]
    }
  }
  
  ### get soils data for this gridcell
  # get indices for soil grid Lat and Lon
  # the closest latitude
  soilLat_i <- which((soilGridLats-Lat) == min(abs(soilGridLats - Lat)))
    
  # the closest longitude 
  soilLon_i <- which((soilGridLons-Long) == min(abs(soilGridLons-Long)))
    #round(soilGridLons,2)==round(Long,2))
  #clay (as a percentage...convert to a fraction by dividing by 100)
  clay_i <- var.get.nc(soils_gridClay, "claytotal", start = c(soilLon_i, soilLat_i,1), 
             count = c(1,1,dim.inq.nc(soils_gridClay, "vertical")$length))/100
  #sand (as a percentage...convert to a fraction by dividing by 100)
  sand_i <- var.get.nc(soils_gridSand, "sandtotal", start = c(soilLon_i, soilLat_i,1), 
                       count = c(1,1,dim.inq.nc(soils_gridSand, "vertical")$length))/100
  #silt (as a percentage...convert to a fraction by dividing by 100)
  silt_i <- var.get.nc(soils_gridSilt, "silttotal", start = c(soilLon_i, soilLat_i,1), 
                       count = c(1,1,dim.inq.nc(soils_gridSilt, "vertical")$length))/100
  #coarse material (as a percentage...convert to a fraction by dividing by 100)
  coarse_i <- var.get.nc(soils_gridCoarse, "fragvol", start = c(soilLon_i, soilLat_i,1), 
                       count = c(1,1,dim.inq.nc(soils_gridCoarse, "vertical")$length))/100
  #thickness
  thickness_i <- var.get.nc(soils_gridThickness, "hzthk", start = c(soilLon_i, soilLat_i,1), 
                       count = c(1,1,dim.inq.nc(soils_gridThickness, "vertical")$length))   
    # units are in cm
  
  bulkdensity_i <- var.get.nc(soils_gridDensity, "dbovendry", start = c(soilLon_i, soilLat_i,1), 
                       count = c(1,1,dim.inq.nc(soils_gridDensity, "vertical")$length)) # units = g/cm3
  
  ## get the depths also (the "vertical_bnds" dimension contains a matrix with 
  # the upper and lower bounds of each depth band--we want the lower bounds)
  
  depths_i <- var.get.nc(soils_gridThickness, "vertical_bnds")[2,]
  ##AES this part below is a test... see what other folks think about this...
  # trim soils data so that there are not NAs (the data stops at the depth for which we have data)
  # also get the depths for the layers included
  depths_i <- depths_i[!is.na(clay_i)]
  clay_i <- clay_i[!is.na(clay_i)] 
  sand_i <- sand_i[!is.na(sand_i)]
  silt_i <- silt_i[!is.na(silt_i)]
  coarse_i <- coarse_i[!is.na(coarse_i)]
  thickness_i <- thickness_i[!is.na(thickness_i)]
  bulkdensity_i <- bulkdensity_i[!is.na(bulkdensity_i)]
  
  # depths 
  ################### ----------------------------------------------------------
  # Part 2 - Sets SW parameters besides weather
  ################### ----------------------------------------------------------
  #setting soilWat specific options
  #sw_in <- new("swInputData") # baseline data # new() creates an empty object of this class, but everything needs to be addedd manually... which may be cumbersome
  sw_in <- swInputData()
  sw_in <- setVeg(sw_in, AllProdInfo, i)
  sw_in <- setSW(sw_in, Lat, Long, clim, 
                 clay_i, sand_i, silt_i, coarse_i, thickness_i, bulkdensity_i)
  #sw_in <- set_soils(sw_in, 2, 35, 35) #AES is done elsewhere in the setSW() function
  sw_in@site@SoilTemperatureFlag <- TRUE # turns on the soil temperature 
  swCarbon_Use_Bio(sw_in) <- FALSE # turns off carbon #turns off CO2 fertilization effects... something we could potentially change
  swCarbon_Use_WUE(sw_in) <- FALSE # turns off Water use efficiency 
  swYears_EndYear(sw_in) <- currYear - 1 # the setting for the historical simulation 
  
  # Soils info formatting ----------------------------------------------------
  # waiting on the proper soils data, this is sort of a placeholder
    #AES how do we define what the depths are?? -- double check 
  SoilsDF <- data.frame(depth_cm = c(1:250),
                        Depth = c(rep('Shallow', 15),
                                  rep('Intermediate', 50), #16 - 65
                                  rep('Deep',185))) # 66 - 250
  # could be problematic if some soils are shallow--maybe should indicate how much depth is represented in each 

  Soils <- data.frame(sw_in@soils@Layers)[,c('depth_cm', 'sand_frac', 'clay_frac')]
  Soils$width <- thickness_i#diff(c(0, Soils$depth_cm))
  SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
  SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])
  
  ################### ----------------------------------------------------------
  # Part 3 - Run SOILWAT Historical!
  ################### ----------------------------------------------------------
  #if(!interactive()) comm.print(pa e('Running Current Site', Site_id, Sys.time()))
  # runs SOILWAT2 for the historical data, and aggregates the results 
  # Note: hypothetically, we could run the historical period once, and then each 
  # month would run this part for the most recent month of historical data before running the anomaly data
  #sw_example <- rSOILWAT2::sw_exampleData
  #AES below is a hack to deal with artificial hard-coded soils data -- are netCDFs that contain soils data that Daniel can share... will just be a question of changing the input code to draw from those files, rather than be hard-coded (these files aren't finalized, but are in the same format )
  rSOILWAT2::swSite_SWClimits(sw_in)[1] <- 100 #SW calculates with negative bars internally, so 10 is -1 mega pascal
  sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
  #getting a warning (2/20), saying that the half-wilting point that SW calculated is lower than the minimum values that is input somewhere else -- probably because we're hard-coding soils  
  
  
  ################ -------------------------------------------------------------
  # FORMAT OUTPUTS    --- Get 18 month median, 10, and 90 for all !!! ---
  ################ -------------------------------------------------------------
  
  HistDataAll <- getOutputs(sw_out, sw_in, SoilsDF, 
                            TimePeriod = 'Historical',
                            calc_EcoVars = TRUE)
  
  # 1 - Get rolling mean -------------------------------------------------------
  HistDataAll1 <- setorder(as.data.frame(HistDataAll[[1]]), Year, Day)
  HistDataRolling <- getRolling(HistDataAll1, TimePeriod = 'Historical')
  
  # 2 - Get climatologies! Daily for 18 months (549 days)  -----------------------------
  
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
  # Question -- where should I save these for easy loading when I need them .......
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
   
  Hist_OConnor2020 <- HistDataAll[[4]]
  
  
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
  Future_OConnor2020 <- dplyr::bind_rows(AnomalyData1[[4]])
  Oconnor_Stats <- formatOConnor2020(Hist_OConnor2020, Future_OConnor2020) 
  
  ################### ----------------------------------------------------------
  # Part 6 - Insert into netCDFs!!!
  ################### ----------------------------------------------------------

  # TO DO: Another netCDF that tracks success and failure
  
  if(!interactive() & isParallel) comm.print('Inserting into netCDFs.', Sys.time())
  
  
  #TO DO: Make this into a function not a script 
  source('./main/implementation/01.2_input-values-into-ncdfs.R') 

}


# Shut down MPI ---------------------------------------------------------------

if (!interactive() & isParallel) {
  
  #   #info.free()
  comm.print('done')
  rSOILWAT2::dbW_disconnectConnection()
  barrier()
  finalize()
  #close netCDF connection?
} else if (!isParallel) {
  
  #close netCDF connection
  
  rSOILWAT2::dbW_disconnectConnection()
  
  apply(matrix(netCDFnames), MARGIN = 1, 
        FUN = function(x) {
        RNetCDF::close.nc(get(x))
        })
}

