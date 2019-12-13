# 
# # for testing
lat <- 35.1266
lng <- -111.5854
soils <- 2
sand <- 50
clay <- 23
comp <- 2
shrubs <- .5
grasses <- .5
forbs <- bg <- trees <- 0

gatherDataAndExecuteSW <- function(lat, lng,
                                   soils, sand = 33, clay = 33,
                                   comp, trees = 0, shrubs = 0.5, grasses = 0.5, forbs = 0, bg = 0){
  
    ################### ----------------------------------------------------------------
    # Part 1 - Getting and formatting weather data for the historical and future runs 
    ################### ----------------------------------------------------------------
  
    # get historical weather data -> using geoknife ... this takes very long and needs to be changed
    
    #wdata <- getWeatherData(lat, lng)
    #write.csv(wdata, 'wdata.csv', row.names = FALSE)
    wdata <- fread('wdata.csv')
    
    # get weather coefficients data for weather generator
    res2 <- getWeatherCoefficientsFromHistorical(wdata)    
    
    # get and integrate weather anomaly data for short-term forecast run
    Anoms <- getFormatAnomalies(lat, lng, wdata)
    res3 <- integrateAnomalyData(res2, Anoms[[1]], Anoms[[2]])
    
    ################### ----------------------------------------------------------------
    # Part 2 - Sets soils and veg 
    ################### ----------------------------------------------------------------
    sw_in0 <- rSOILWAT2::sw_exampleData # baseline data
    
    # set whether soils should be extracted from 250m data or chosen by user
    sw_in0 <- set_soils(sw_in0, soils, sand, clay)
    
    #3 set whether composition should be predicted from climate or chosen by user
    sw_in0 <- set_comp(sw_in0, comp, trees, shrubs, grasses, forbs, bg)
    
    ################### ----------------------------------------------------------------
    # Part 3 - Run Soilwat
    ################### ----------------------------------------------------------------
    
    # Run 0 - with normals/historical data
    swCarbon_Use_Bio(sw_in0) <- FALSE
    swCarbon_Use_WUE(sw_in0) <- FALSE
    swYears_EndYear(sw_in0) <- 2015
    
    weath <- dbW_dataframe_to_weatherData(wdata[wdata$Year %in% c(1979:2015),])
    sw_out2 <- sw_exec(inputData = sw_in0, weatherList = weath)
  #  saveRDS(object = sw_out2, file = 'outNormal.rds')
    
    # Run 1 - with anomaly data
    swYears_EndYear(sw_in0) <- 2010
    swMarkov_Prob(sw_in0) <- res2[["mkv_doy"]]
    swMarkov_Conv(sw_in0) <- res2[["mkv_woy"]]
    swWeather_UseMarkov(sw_in0) <- TRUE
    slot(sw_in0, "weatherHistory") <- list(new("swWeatherData"))
    sw_out1 <- sw_exec(inputData = sw_in0)
   # saveRDS(object = sw_out1, file = 'outAnomaly.rds')
    
    ################### ----------------------------------------------------------------
    # Part 4 - Returned formatted outputs
    ################### ----------------------------------------------------------------
    
    ###### Set up
    SoilsDF <- data.frame(depth_cm = c(1:250), Depth = c(rep('Shallow', 15), rep('Intermediate', 45), rep('Deep',190)))
    Soils <- data.frame(sw_in0@soils@Layers)
    Soils <- Soils[,c('depth_cm', 'sand_frac', 'clay_frac')]
    Soils$width <- diff(c(0, Soils$depth_cm))
    SoilsDF <- plyr::join(Soils, SoilsDF)
    SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])
    
    ##### Future/Anomaly Data Data ---------------------------------------------
   
     Temp1 <- data.frame(sw_out1@TEMP@Day)
    Temp1 <- setDT(Temp1)[, .(Temp = mean(avg_C)), .(Day)]
    PPT1 <-  data.frame(sw_out1@PRECIP@Day)
    PPT1 <- setDT(PPT1)[,.(PPT = sum(ppt)),.(Day)]
    # SWP ---------------------------------------------------
    # Step 1 - Get weighted mean across depths for VWC
    VWC1 <-  data.frame(sw_out1@VWCMATRIC@Day)
    VWC1$Month <- month(strptime(paste(VWC1$Year, VWC1$Day, sep = '-'), format = "%Y-%j"))
    VWC1 <- melt(VWC1, id.vars = c('Year', 'Month', 'Day'))
    VWC1 <- plyr::join(VWC1, SoilsDF)
    VWC1 <- setDT(VWC1)[,.(VWC = weighted.mean(value, width), sand = weighted.mean(sand_frac, width), clay = weighted.mean(clay_frac, width)),
                        .(Year, Month, Day, Depth)]
    
    # Step 2 -  SWP for every year
    VWC_AllYears1 <- VWC1[,.(SWP_mean = rSFSW2::VWCtoSWP(VWC, sand, clay)),.(Year, Month, Day, Depth)]
    VWC_AllYears1$Type <- 'SWP - Future'
    VWC_AllYears1$Date <- as.Date(VWC_AllYears1$Day, origin = '2019-01-01')
    
    # Step 3 - Mean SWP
    SWP1 <- VWC1
    SWP1 <- SWP1[, .(VWC = mean(VWC)),  .( Day, Depth, sand, clay)]
    SWP1$SWP_mean <- rSFSW2::VWCtoSWP(SWP1$VWC, SWP1$sand, SWP1$clay)[,1]
    
    # Join up
    AnomalyData <- plyr::join(Temp1, PPT1)
    AnomalyData <- plyr::join(AnomalyData, SWP1)
    AnomalyData$Type <- 'Mean SWP - Future'
    AnomalyData$Date <- as.Date(AnomalyData$Day, origin = '2019-01-01')
    
    
    ##### Hist/Normals Data ---------------------------------------------
    
    Temp2 <- data.frame(sw_out2@TEMP@Day)
    Temp2 <- setDT(Temp2)[, .(Temp = mean(avg_C)), .(Day)]
    PPT2 <-  data.frame(sw_out2@PRECIP@Day)
    PPT2 <- setDT(PPT2)[,.(PPT = sum(ppt)),.(Day)]
    
    # SWP ---------------------------------------------------
    # Step 1 - Get weighted mean across depths for VWC
    VWC2 <-  data.frame(sw_out2@VWCMATRIC@Day)
    VWC2$Month <- month(strptime(paste(VWC2$Year, VWC2$Day, sep = '-'), format = "%Y-%j"))
    VWC2 <- melt(VWC2, id.vars = c('Year', 'Month', 'Day'))
    
    VWC2 <- plyr::join(VWC2, SoilsDF)
    VWC2 <- setDT(VWC2)[,.(VWC = weighted.mean(value, width), sand = weighted.mean(sand_frac, width), clay = weighted.mean(clay_frac, width)),
                        .(Year, Month, Day, Depth)]
    # Step 2 -  SWP for every year
    VWC_AllYears2 <- VWC2[,.(SWP_mean = rSFSW2::VWCtoSWP(VWC, sand, clay)),.(Year, Month, Day, Depth)]
    VWC_AllYears2$Type <- 'SWP - Normal'
    VWC_AllYears2$Date <- as.Date(VWC_AllYears2$Day, origin = '2019-01-01')
    
    # Step 3 - Mean SWP
    SWP2 <- VWC2
    SWP2 <- SWP2[, .(VWC_mean = mean(VWC), VWC_min = mean(VWC) - sd(VWC), VWC_max = mean(VWC) + sd(VWC)),  .(Day, Depth, sand, clay)]
    SWP2 <- SWP2[,.(SWP_mean = rSFSW2::VWCtoSWP(VWC_mean, sand, clay),
                    SWP_min = rSFSW2::VWCtoSWP(VWC_min, sand, clay),
                    SWP_max = rSFSW2::VWCtoSWP(VWC_max, sand, clay)),.(Day, Depth)]
    
    HistData <- plyr::join(Temp2, PPT2)
    HistData <- plyr::join(HistData, SWP2)
    HistData$Type <- 'Mean SWP - Normal'
    HistData$Date <- as.Date(HistData$Day, origin = '2019-01-01')
    
    return(list(AnomalyData, HistData, VWC_AllYears1, VWC_AllYears2))
    
    }