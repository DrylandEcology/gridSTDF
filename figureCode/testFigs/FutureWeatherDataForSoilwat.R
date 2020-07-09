wdata <- fread('ExampleData/wdata.csv')
wdata$Month <- month(wdata$Date)

# Step 4 ----------------------------------------------------------------------------------------------
# Create future weather / integrate anomaly data into historical weather ----------------------------------------------------------
years <- 1981:2011
wdata2 <- wdata[wdata$Year %in% years, ]
weathAnomAll <- suppressWarnings(integrateAnomalyData(wdata2, yearlydat))
weathAnomAll$Month <- month(as.Date(strptime(paste(weathAnomAll$Year, weathAnomAll$DOY), format="%Y %j"), format="%m-%d-%Y"))
  
wdataTest <- wdata[,.(MaxTempMean = mean(Tmax_C)), .(Month, Year)]
weathAnomAllTest <- weathAnomAll[,.(MaxTempMean2 = mean(Tmax_C)), .(Month, Year)]
weathTest <- plyr::join(wdataTest, weathAnomAllTest)
weathTest$Diff <- weathTest$MaxTempMean2 - weathTest$MaxTempMean
weathTest <- plyr::join(weathTest, yearlydat)

# Make weather data for one simulation
## Three years worth of data: 1) One year ago 2) Observed until today's date 3) Future data integrated with historical data (weath Anom)
## Run 30 times, where each time corresponds to a different year in the historical record

### year 1 - The year prior to current year's observed data
year1 <- wdata[wdata$Year == (currYear - 1), c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]

# observed weather -----------------------------------------------
thisYearObservedWData <- wdata[wdata$Year == currYear, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
days <- dim(thisYearObservedWData)[1]

y <- 1982
#for(y in 1981:2010){
  #   print(y)
  ### ---------
  ### year 2 - observed data for this year until today's date and then future, weathAnom data
  ### ---------
  year2 <- year2Fut <- weathAnomAll[weathAnomAll$Year == y,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm') ]
  year2$Year <- as.integer(currYear) # change year
  
  # add observed data where appropriate
  year2[1:currDOY,] <- thisYearObservedWData[1:currDOY,] # this works even when currDOY is 366 and year2 DF only has 365 rows
  
  # if "year2" is 365 days but current year is 366 days 
  if(days == 366 & nrow(year2) != 366) {
    year2 <- rbind(year2, year2[365,])
    year2$DOY[366] <- 366
  }
  
  ### ---------
  ## year 3 ... forecasts that run into next year (aka 2021) and then scratch data for the rest of 2021
  ### ---------
  year3 <-  weathAnomAll[weathAnomAll$Year == y + 1, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]  
  if(days == 366) year3 <- year3[1:365,] #if this year is 366 days, next year or the year after that needs to be 365
  year3$Year <- as.integer(currYear + 1)
  
  weathAnomOneSim <- rbind(year1, year2, year3)
  
  
  
  # PLot
  weathAnomOneSim2 <- weathAnomOneSim
  weathAnomOneSim2$Date <- as.Date(strptime(paste(weathAnomOneSim2$Year, weathAnomOneSim2$DOY), format="%Y %j"), format="%m-%d-%Y")
  wdataY1 <- wdata[wdata$Year %in% c(y)]
  wdataY1$Year <- 2020
  wdataY2 <- wdata[wdata$Year %in% c(y+ 1)]
  wdataY2$Year <- 2021
  wdataNew <- rbind(wdataY1, wdataY2)
  wdataNew$Date <- as.Date(strptime(paste(wdataNew$Year, wdataNew$DOY), format="%Y %j"), format="%m-%d-%Y")
  
  
Plot1 <- ggplot() +
    geom_line(data = weathAnomOneSim2, aes(Date, Tmax_C, color = as.factor(Year)))+
    #geom_line(data = wdataNew, aes(Date, Tmax_C)) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    geom_vline(xintercept = Sys.Date())
  
  
  # run SOILWAT2 for future years ----------------------------------------------------------
weathAnomOneSim <- dbW_dataframe_to_weatherData(weathAnomOneSim, round = 4)
  swYears_EndYear(sw_in0) <- currYear + 1
  swYears_StartYear(sw_in0) <- currYear - 1
  
  sw_out <- sw_exec(inputData = sw_in0, weatherList = weathAnomOneSim)
  
  SWOut <- data.frame(sw_out@TEMP@Day)
  SWOut$Date <- as.Date(strptime(paste(SWOut$Year, SWOut$Day), format="%Y %j"), format="%m-%d-%Y")
  
  Plot1 + geom_line(data = SWOut, aes(Date, max_C), color = 'purple', lty = 'dashed')
  