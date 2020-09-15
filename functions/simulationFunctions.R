library(rSOILWAT2)
library(rSW2funs)
library(splines)
# data formatting
library(data.table)
library(lubridate)
library(raster)
library(zoo)
library(caTools)
# app
library(plumber)
library(rvest)

#source('functions/weatherFunctions.R')
source('functions/soilsAndComp.R')
source('functions/Outputs.R')
source('functions/HelperFunctions.R')
source('functions/ecoIndicators.R')

#
# lat <- 35.1983
# lng <- -111.6513
# soils <- 2
# sand <- 50
# clay <- 15
# comp <- 2
# grasses <- .5
# shrubs <- .5
# trees <- 0
# forbs <- 0
# bg <- 0

#* gather user data, execute soilwat simulations, and return outputs
#* @param lat latitude of site
#* @param long longitude of site
#* @param soils binary decision of user 1 is extract from a gridded source 2 is user input
#* @param sand user input of sand texture
#* @param clay user input of clay texture

#* @get /gatherDataAndExecuteSW
 gatherDataAndExecuteSW <- function(lat, lng,
                                soils, sand = 33, clay = 33, comp = 1) {

    ################### ----------------------------------------------------------------
    # Part 0 - Setup - format data from HTTP request, get dates
    ################### ----------------------------------------------------------------
    lat <- as.numeric(lat)
    lng <- as.numeric(lng)
    print(lat)
    print(lng)

    sand <- as.numeric(sand)
    clay <- as.numeric(clay)

    currDOY <- yday(Sys.Date())
    currMonth <- month(Sys.Date())
    currYear <- year(Sys.Date())

    ################### ----------------------------------------------------------------
    # Part 1 - Getting and formatting weather data for the historical and future runs
    ################### ----------------------------------------------------------------

    # get historical weather data -> using geoknife ... this takes very long and needs to be changed

    print(Sys.time())
    wdata <- getWeatherData(lat, lng, currYear,
                            dir = '~/Desktop/www.northwestknowledge.net/metdata/data/')
    print(Sys.time())
    # write.csv(wdata2, 'ExampleData/wdata.csv', row.names = FALSE)
    #wdata <- fread('ExampleData/wdata.csv')

    ################### ----------------------------------------------------------------
    # Part 2 - Sets soils and veg and lat
    ################### ----------------------------------------------------------------
    sw_in0 <- rSOILWAT2::sw_exampleData # baseline data

    # set whether soils should be extracted from 250m data or chosen by user
    sw_in0 <- set_soils(sw_in0, soils, sand, clay)

    #3 set whether composition should be predicted from climate or chosen by user
    sw_in0 <- set_comp(sw_in0, comp, trees, shrubs, grasses, forbs, bg)

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
  # write.csv(SoilsDF, 'Tests/TestExample/SoilsDF.csv')
    # --------------------------------------------------------------------------
    # Run 1 - with observed historical data ------------------------------------
    # --------------------------------------------------------------------------
    print('Running Historical')
    swCarbon_Use_Bio(sw_in0) <- FALSE
    swCarbon_Use_WUE(sw_in0) <- FALSE
    swYears_EndYear(sw_in0) <- currYear - 1

    weath <- dbW_dataframe_to_weatherData(wdata[wdata$Year %in% c(1979:(currYear - 1)),
                                                c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
    sw_out0 <- sw_exec(inputData = sw_in0, weatherList = weath, quiet = FALSE)
    HistDataAll <- getOutputs(sw_out0, sw_in0)

    # format outputs
    HistDataAll1 <- setorder(HistDataAll[[1]], Year, Day)
    #fwrite(HistDataAll1, 'Tests/TestExample/HistData.csv')

    HistDataAll1 <- getRolling(HistDataAll1)

    HistData_Norm_Stats <- HistDataAll1[HistDataAll1$Year %in% 1981:2010, ] # historical normal
    HistData_Norm_Stats <- makeDateMonthDat(HistData_Norm_Stats, 'Day')     # Get date without the year
    HistData_Norm_Stats$Year <- HistData_Norm_Stats$Day <- NULL
    HistData_Norm_Stats <- setnames(setDT(HistData_Norm_Stats)[ ,sapply(.SD, function(x) list(med=median(x),
                                                                                        x10=quantile(x, .1, na.rm = TRUE),
                                                                                        x90 = quantile(x, .9, na.rm = TRUE))),
                                                          .(Date)],
                                 c('Date', sapply(names(HistData_Norm_Stats)[-c(11)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!
    HistData_Norm_Stats <- getSWP(HistData_Norm_Stats, SoilsDF)
    HistData_Norm_Stats <- setorder(HistData_Norm_Stats, Date)

    # eco vars ------------------------------------------------------------------
    Hist_Shriver2018 <- data.table(Year = HistDataAll[[2]]$PlantedinYear,
        Prob = p_Shriver2018(HistDataAll[[2]]$Temp_mean, HistDataAll[[2]]$VWC_mean))

    Hist_GISSM <- data.table(HistDataAll[[3]])

    #  --------------------------------------------------------------------------
    # Run 2 - with future anomaly data
    #  --------------------------------------------------------------------------
    print('Running Future')
    print(Sys.time())
    AnomalyData1 <- runFutureSWwithAnomalies(lat, lng,  sw_in0, wdata, res2, n = 1, SoilsDF,
                                             currDOY, currMonth, currYear)

    AllOut <- AnomalyData1[[1]]
    #fwrite(AllOut, 'Tests/TestExample/FutureData.csv')
    MonthlyAnoms <- AnomalyData1[[3]]

    AnomRunStats <- formatOutputsFuture(AllOut, SoilsDF)

    # eco vars ------------------------------------------------------------------
    Future_Shriver2018 <- data.table(Year = AnomalyData1[[2]]$PlantedinYear, run = AnomalyData1[[2]]$run,
                                     Prob =  p_Shriver2018(AnomalyData1[[2]]$Temp_mean, AnomalyData1[[2]]$VWC_mean))

    Future_GISSM <- data.table(AnomalyData1[[3]])

    # format ecovars for writing out -------------------------------------------
    Shriver_Stats <- formatShriver2018(Hist_Shriver2018, Future_Shriver2018, currYear)
    GISSM_Stats <- formatGISSM(Hist_GISSM, Future_GISSM)

    ################### ----------------------------------------------------------------
    # Part 4 - Write out formatted outputs
    ################### ----------------------------------------------------------------
    #fwrite(HistData_Norm_Stats, 'ExampleData/HistData_Norm_Stats.csv')
    # fwrite(MonthlyAnoms, 'ExampleData/MonthlyAnoms.csv')
    #fwrite(AnomRunStats, 'ExampleData/AnomRun_Stats.csv')
    #
    # fwrite(Hist_Shriver2018, 'ExampleData/Hist_Shriver2018.csv')
    # fwrite(Future_Shriver2018, 'ExampleData/Future_Shriver2018.csv')
    # #fwrite(Hist_GISSM, 'ExampleData/Hist_GISSM.csv')
    # #fwrite(Future_GISSM, 'ExampleData/Future_GISSM.csv')
    return(list(HistData_Norm_Stats, AnomRunStats, Shriver_Stats, GISSM_Stats))#, HistDataAll1))

}
