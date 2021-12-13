#* gather user data, execute soilwat simulations, and return outputs
#* @param lat latitude of site
#* @param lng longitude of site
#* @param soils binary decision of user 1 is extract from a gridded source 2 is user input
#* @param sand user input of sand texture
#* @param clay user input of clay texture

#* @get /gatherDataAndExecuteSW
 gatherDataAndExecuteSW <- function(lat, lng,
                                soils, sand = 33, clay = 33,
                                write = FALSE, verbose = TRUE) {


    ################### ----------------------------------------------------------------
    # Part 0 - Setup - format data from HTTP request, get dates
    ################### ----------------------------------------------------------------
    if(getwd() != "/usr/local/app/STDF/shorttermdroughtforecaster") {
        setwd("/usr/local/app/STDF/shorttermdroughtforecaster")
    }

    print(paste(getwd()))

    lat <- as.numeric(lat)
    lng <- as.numeric(lng)
    print(lat)
    print(lng)

    sand <- as.numeric(sand)
    clay <- as.numeric(clay)

    currDOY <- yday(Sys.Date())
    currMonth <- month(Sys.Date())
    currYear <- year(Sys.Date())
    currDate <- Sys.Date()
    todayMonthDay <- format(Sys.Date() , format="%m-%d")

    ################### ----------------------------------------------------------------
    # Part 1 - Getting and formatting weather data for the historical and future runs
    ################### ----------------------------------------------------------------

    if(verbose) print(paste('Formatting Weather Data', Sys.time()))
    wdata <- getWeatherData(lat, lng, currYear,
                            dir = '../WeatherData/www.northwestknowledge.net/metdata/data/')

    lastWeatherDate <- wdata[[2]]
    wdata <- wdata[[1]]

    weath <- rSOILWAT2::dbW_dataframe_to_weatherData(wdata[wdata$Year %in% c(1979:(currYear - 1)),
                                                c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)

    ################### ----------------------------------------------------------------
    # Part 2 - Sets soils and veg and lat
    ################### ----------------------------------------------------------------
    sw_in0 <- rSOILWAT2::sw_exampleData # baseline data

    # set whether soils should be extracted from 250m data or chosen by user
    sw_in0 <- set_soils(sw_in0, soils, sand, clay)

    #3 predict composition from climate
    sw_in0 <- set_comp(sw_in0, weath)

    # 4 set latitude. Used in GISSM calculations
    swSite_IntrinsicSiteParams(sw_in0)[["Latitude"]] <- lat * pi/180

    ################### ----------------------------------------------------------------
    # Part 3 - Run Soilwat
    ################### ----------------------------------------------------------------

    # Soils info formatting ----------------------------------------------------
    SoilsDF <- data.frame(depth_cm = c(1:250),
                          Depth = c(rep('Shallow', 15), rep('Intermediate', 50), rep('Deep',185)))

    Soils <- data.frame(sw_in0@soils@Layers)
    Soils <- Soils[,c('depth_cm', 'sand_frac', 'clay_frac')]
    Soils$width <- diff(c(0, Soils$depth_cm))
    SoilsDF <- merge(Soils, SoilsDF, by = 'depth_cm')
    SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])

    # --------------------------------------------------------------------------
    # Run 1 - with observed historical data ------------------------------------
    # --------------------------------------------------------------------------
    if(verbose) print(paste('Running Historical', Sys.time()))
    swCarbon_Use_Bio(sw_in0) <- FALSE
    swCarbon_Use_WUE(sw_in0) <- FALSE
    swYears_EndYear(sw_in0) <- currYear - 1

    sw_out0 <- sw_exec(inputData = sw_in0, weatherList = weath, quiet = TRUE)
    HistDataAll <- getOutputs(sw_out0, sw_in0, SoilsDF, TimePeriod = 'Historical')

    # format outputs
    HistDataAll1 <- setorder(HistDataAll[[1]], Year, Day)
    HistDataRolling <- getRolling(HistDataAll1)

    # Climatologies for plotting
    HistData_Norm_Stats1 <- getHistoricalClimatology(HistDataRolling, 1981, 2011, SoilsDF)
    HistData_Norm_Stats2 <- getHistoricalClimatology(HistDataRolling, 1981, 2010, SoilsDF)
    HistData_Norm_Stats3 <- getHistoricalClimatology(HistDataRolling, 1982, 2011, SoilsDF)

    HistDataNormMean_18MNs <- get18MonthClimatologicalRecord(HistData_Norm_Stats1,
                                                             HistData_Norm_Stats2,
                                                             HistData_Norm_Stats3,
                                                             currDate, currMonth,
                                                             currYear, todayMonthDay)
    # Monthlys for future delta calculations
    HistData_MonthlyMeans_2 <- formatOutputs_Monthlys(HistDataAll1, SoilsDF, 'historical', 1981, 2010,
                                                      currDate, todayMonthDay, currYearClimatology = TRUE)
    HistData_MonthlyMeans_3 <- formatOutputs_Monthlys(HistDataAll1, SoilsDF, 'historical', 1982, 2011)

    # make one year record
    HistData_MonthlyMeans <- formatHistoricalMonthlys(HistData_MonthlyMeans_2,
                                                      HistData_MonthlyMeans_3,
                                                      currYear, todayMonthDay)

    # eco vars ------------------------------------------------------------------
    Hist_Shriver2018 <- data.table(Year = HistDataAll[[2]]$PlantedinYear,
        Prob = p_Shriver2018(HistDataAll[[2]]$Temp_mean, HistDataAll[[2]]$VWC_mean))

    Hist_GISSM <- data.table(HistDataAll[[3]])
    
    Hist_OConnor2020 <- HistDataAll[[4]]

    #  --------------------------------------------------------------------------
    # Run 2 - with future anomaly data
    #  --------------------------------------------------------------------------
    if(verbose) print(paste('Running Future', Sys.time()))
    AnomalyData1 <- runFutureSWwithAnomalies(lat, lng,  sw_in0, wdata, n = 30, SoilsDF,
                                             currDOY, currMonth, currYear, currDate)

    if(verbose) print(paste('Formatting Outputs', Sys.time()))

    AllOut <- AnomalyData1[[1]]
    MonthlyAnoms <- AnomalyData1[[5]]

    # Recent past (6 months prior to current): included in 'future' runs --------
    AnomRunStats <- formatOutputsFuture(AllOut, SoilsDF, currDate)
    AnomRunStats <- AnomRunStats[AnomRunStats$Date < lastWeatherDate, ]

    # Upcoming year (current date + 1 year)
    AnomRunStats2 <- formatOutputs_Monthlys(AllOut, SoilsDF, 'future', currDate = currDate)

    # eco vars ------------------------------------------------------------------
    Future_Shriver2018 <- data.table(Year = AnomalyData1[[2]]$PlantedinYear, run = AnomalyData1[[2]]$run,
                                     Prob =  p_Shriver2018(AnomalyData1[[2]]$Temp_mean, AnomalyData1[[2]]$VWC_mean))

    Future_GISSM <- data.table(AnomalyData1[[3]])
    
    Future_OConnor2020 <- data.table(AnomalyData1[[4]])

    ################### ----------------------------------------------------------------
    # Part 4 - Calculate deltas, formout outputs
    ################### ----------------------------------------------------------------

    # calculate deltas and approx and format
    Vars <- c('avg_C', 'ppt', 'VWC.Shallow', 'VWC.Intermediate', 'VWC.Deep',
              'SWP.Shallow', 'SWP.Intermediate', 'SWP.Deep')

    AllVarData <- data.frame(Date = as.Date((currDate-183):(currDate+365)))

    for(v in seq(Vars)){
        OneVarData <- suppressMessages(calcDeltasApproxAndFormat(HistData_Norm_Stats1, HistData_MonthlyMeans,
                                                                 as.data.frame(HistDataNormMean_18MNs),
                                                                 AnomRunStats, AnomRunStats2,
                                                                 Vars[v], currDate, todayMonthDay, currYear,
                                                                 lastWeatherDate))

        AllVarData <- merge(AllVarData, OneVarData)
    }

    # format ecovars for writing out -------------------------------------------
    Shriver_Stats <- formatShriver2018(Hist_Shriver2018, Future_Shriver2018, currYear)
    GISSM_Stats <- formatGISSM(Hist_GISSM, Future_GISSM)
    Oconnor_Stats <- formatOConnor2020(Hist_OConnor2020, Future_OConnor2020)

    ################### ----------------------------------------------------------------
    # Part 5 - Write out formatted outputs
    ################### ----------------------------------------------------------------
    if(write){
        fwrite(MonthlyAnoms, 'ExampleData/MonthlyAnoms.csv')
        fwrite(data.frame(lastWeatherDate), 'ExampleData/lastWeatherDate.csv')
        fwrite(AllVarData, 'ExampleData/AllVarData.csv')
        fwrite(Shriver_Stats, 'ExampleData/Shriver_Stats.csv')
        fwrite(GISSM_Stats, 'ExampleData/GISSM_Stats.csv')
        fwrite(Oconnor_Stats, 'ExampleData/Oconnor_Stats.csv')
    }

    if(verbose) print(paste('Done', Sys.time()))

    return(list(AllVarData, Shriver_Stats, GISSM_Stats, Oconnor_Stats, lastWeatherDate, currDate))#, HistDataAll1))

}
