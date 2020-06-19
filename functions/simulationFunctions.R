set.seed(5)
# # for testing
library(rSFSW2)
library(rSOILWAT2)
library(splines)
# data formatting
library(data.table)
library(lubridate)
library(raster)
library(zoo)
library(caTools)
# weather
library(geoknife)

functionFiles <- list.files('functions', full.names = TRUE)
sapply(functionFiles[c(2,4,5)], source)
#debug(integrateAnomalyData)
lat <- 35.1266
lng <- -111.5854
soils <- 2
sand <- 50
clay <- 15
comp <- 2
shrubs <- .5
grasses <- .5
forbs <- bg <- trees <- 0

#* gather user data, execute soilwat simulations, and return outputs
#* @param lat latitude of site
#* @param long longitude of site
#* @param soils binary decision of user 1 is extract from a gridded source 2 is user input
#* @param sand user input of sand texture
#* @param clay user input of clay texture
#* @param comp binary decision of user 1 is generate composition from climate 2 is user inputs
#* @param trees user input of tree composition
#* @param shrubs user input of shrub composition
#* @param grasses user input of grass composition
#* @param forbs user input of forb composition
#* @param bg user input of bareground composition
#* @get /gatherDataAndExecuteSW
# gatherDataAndExecuteSW <- function(lat, lng,
#                                    soils, sand = 33, clay = 33,
#                                    comp, trees = 0, shrubs = 0.5, grasses = 0.5, forbs = 0, bg = 0){

    ################### ----------------------------------------------------------------
    # Part 0 - format data from HTTP request
    ################### ----------------------------------------------------------------
    lat <- as.numeric(lat)
    lng <- as.numeric(lng)
    print(lat)
    print(lng)

    ################### ----------------------------------------------------------------
    # Part 1 - Getting and formatting weather data for the historical and future runs
    ################### ----------------------------------------------------------------

    # get historical weather data -> using geoknife ... this takes very long and needs to be changed

     # print(Sys.time())
     # wdata <- getWeatherData(lat, lng)
     #  print(Sys.time())
    #write.csv(wdata, 'ExampleData/wdata.csv', row.names = FALSE)
    wdata <- fread('ExampleData/wdata.csv')

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

    # Soils info formatting ------------------------------------------------------
    SoilsDF <- data.frame(depth_cm = c(1:250),
                          Depth = c(rep('Shallow', 15), rep('Intermediate', 45), rep('Deep',190)))
    Soils <- data.frame(sw_in0@soils@Layers)
    Soils <- Soils[,c('depth_cm', 'sand_frac', 'clay_frac')]
    Soils$width <- diff(c(0, Soils$depth_cm))
    SoilsDF <- merge(Soils, SoilsDF)
    SoilsDF$variable <- paste0('Lyr_',1:dim(SoilsDF)[1])

    # Run 1 - with observed historical data
    swCarbon_Use_Bio(sw_in0) <- FALSE
    swCarbon_Use_WUE(sw_in0) <- FALSE
    swYears_EndYear(sw_in0) <- year(Sys.Date()) - 1

    weath <- dbW_dataframe_to_weatherData(wdata[wdata$Year %in% c(1979:2019), c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
    sw_out0 <- sw_exec(inputData = sw_in0, weatherList = weath)
    HistDataAll <- getOutputs(sw_out0)
    
    # format outputs
    HistDataNormMean <- HistDataAll[HistDataAll$Year %in% 1981:2010, ]
    HistDataNormMean$Year <- NULL
    HistDataNormMean <- setnames(setDT(HistDataNormMean)[ ,sapply(.SD, function(x) list(med=median(x),
                                                                                        x10=quantile(x, .1, na.rm = TRUE),
                                                                                        x90 = quantile(x, .9, na.rm = TRUE))),
                                                          .(Day)],
                                 c('Day', sapply(names(HistDataNormMean)[-c(1)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!
    
    #  -------------------------------------------------------------------------------------------------
    # Run 2 - with future anomaly data
    AnomalyData1 <- (runFutureSWwithAnomalies(lat, lng,  sw_in0, wdata, res2, n = 30, SoilsDF))
    
    
    ################### ----------------------------------------------------------------
    # Part 4 - Returned formatted outputs
    ################### ----------------------------------------------------------------
    AllOut <- AnomalyData1[[1]]
    MonthlyAnoms <- AnomalyData1[[2]]
    
    #FutureData <- AllOut[AllOut$Date > c(Sys.Date()), ]
    
    AllOut <- setorder(AllOut, run, Date)
    AllOut$run <- AllOut$Year <- AllOut$Day <- NULL 
    
    AnomRunStats <- setnames(setDT(AllOut)[, sapply(.SD, function(x) list(med=median(x), 
                                                                          x10=quantile(x, .1, na.rm = TRUE), 
                                                                          x90 = quantile(x, .9, na.rm = TRUE))),
                                           .(Date)],
                             c('Date', sapply(names(AllOut)[-c(11)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!
    
    
    AnomRunStats <- AnomRunStats[AnomRunStats$Date > Sys.Date() - 183, ] # 6 month lead ins 
    AnomRunStats$Time <- ifelse(AnomRunStats$Date < Sys.Date(), 'Observed', 'Future')
    
    # write out consolidated data ---------------------------------------------------------------
    fwrite(HistDataNormMean, 'ExampleData/HistDataNormMean.csv') 
    #fwrite(MonthlyAnoms, 'ExampleData/MonthlyAnoms.csv')
    fwrite(AnomRunStats, 'ExampleData/AnomRunStats.csv')
    
    #    return(list(AnomalyData, HistDataAll)) # AnomalyData, HistData, VWC_AllYears1, VWC_AllYears2

#}

