getWeatherData <- function(lat, lng) {
  
  stencil <- simplegeom(c(lng, lat))
  fabric = webdata(url='https://cida.usgs.gov/thredds/dodsC/UofIMETDATA')
  variables(fabric) <- c('precipitation_amount', 'min_air_temperature', 'max_air_temperature')
  times(fabric) <- c('1979-01-01', paste(Sys.Date()))
  job <- geoknife(stencil, fabric, wait = TRUE)
  
  # format
  data <- result(job)
  data$statistic <- NULL
  
  wdata <- reshape2::dcast(data, DateTime ~ variable, value.var = 'bufferedPoint')
  #   Year DOY Tmax_C Tmin_C PPT_cm
  wdata$Year <- year(wdata$DateTime)
  wdata$DOY <- yday(wdata$DateTime)
  
  wdata$DateTime <- NULL
  wdata <- wdata[,c('Year', 'DOY', 'max_air_temperature', 'min_air_temperature', 'precipitation_amount' )]
  names(wdata) <- c('Year', 'DOY','Tmax_C', 'Tmin_C', 'PPT_cm')
  
  # 2019 and 2020 for temp are incorrect ...
  summary(wdata)
  wdata2 <- wdata[wdata$Year %in% 1979:2018, ]
  
  # Make data.frame of just years and days -------------------------------------------
  currYear <- year(Sys.time())
  wdataFrame <- data.frame(Date = seq(from = as.Date('1979-01-01'), 
                                      to = as.Date(paste0(year(Sys.Date()),'-12-31')), by="day"))
  wdataFrame$Year <- year(wdataFrame$Date)
  wdataFrame$DOY <- yday(wdataFrame$Date)
  
  # Join with weather data to make NAs -------------------------------------------
  wdata2 <- plyr::join(wdataFrame, wdata2)
  
  # Get info from netcdfs ....
  #https://www.northwestknowledge.net/metdata/data/
  # Tmax_C
  files <- list.files('~/Desktop/CDI_2019/wdatanc/tmax/', full.names = TRUE)
  for(f in 1:length(files)){
    year <- as.numeric(substr(files[f],  nchar(files[f]) - 6, nchar(files[f]) -3))
    
    nc <- suppressWarnings(brick(files[f], varname = 'air_temperature'))
    vals <- extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
    wdata2[wdata2$Year == year, 'Tmax_C'][1:length(vals)] <- vals
    }
  # Tmin_C
  files <- list.files('~/Desktop/CDI_2019/wdatanc/tmin/', full.names = TRUE)
  for(f in 1:length(files)){
    year <- as.numeric(substr(files[f],  nchar(files[f]) - 6, nchar(files[f]) -3))
    
    nc <- suppressWarnings(brick(files[f], varname = 'air_temperature'))
    vals <- extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
    wdata2[wdata2$Year == year, 'Tmin_C'][1:length(vals)] <- vals
    }
  
  #PPT_cm
  files <- list.files('~/Desktop/CDI_2019/wdatanc/pr/', full.names = TRUE)
  for(f in 1:length(files)){
    year <- as.numeric(substr(files[f],  nchar(files[f]) - 6, nchar(files[f]) -3))
    
    nc <- suppressWarnings(brick(files[f], varname = 'precipitation_amount'))
    vals <- extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
    wdata2[wdata2$Year == year, 'PPT_cm'][1:length(vals)] <- vals
      }
  
  # Convert 
  wdata2$Tmax_C <- wdata2$Tmax_C - 273.15
  wdata2$Tmin_C <- wdata2$Tmin_C - 273.15
  wdata2$PPT_cm <- wdata2$PPT_cm/10
  
  # if there isn't 365 days in each year ... na.locf for temp and put 0 for ppt?
  # fill in missing with weather generator when running SOILWAT
  wdata2$Tmax_C <- zoo::na.locf(wdata2$Tmax_C)
  wdata2$Tmin_C <- zoo::na.locf(wdata2$Tmin_C)
  wdata2[is.na(wdata2$PPT_cm), 'PPT_cm'] <- 0
  
  return(wdata2)
}

runFutureSWwithAnomalies <- function(lat, lng, sw_in0, wdata, res2, n, SoilsDF){
  
  AnomSave <- data.frame() # for testing
  AllOut <- data.frame()
  TempMonthlyAnomsAll <- data.frame()  # for testing
  PPTMonthlyAnomsAll <- data.frame()  # for testing
  
  # Determine Region from coordinates and shapefile ------------------------------------------
  CD102 <- shapefile(x = 'CD102/CD102.shp')
  points <- data.frame(x = lng, y = lat)
  
  coordinates(points) <- ~ x + y 
  proj4string(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  CDRegion <- as.numeric(over(points, CD102)$ID4)
  
  # Read in and subset anomaly data -----------------------------------------------------------
  TempMonthlyAnomsWhole <- fread('CurrentAnomalyTempData.csv')
  TempMonthlyAnoms <- subset(TempMonthlyAnomsWhole, CD == CDRegion)

  PPTMonthlyAnomsWhole <- fread('CurrentAnomalyPPTData.csv')
  PPTMonthlyAnoms <- subset(PPTMonthlyAnomsWhole, CD == CDRegion)

  # Establish date and time info -------------------------------------------------------------
  
  # Establish current month and how the 'LEAD's relate to months
  # A LEAD of 1 relates to the forecast beginning where the currmonth + 1
  
  currMonth <- month(Sys.Date())
  currDOY <- yday(Sys.Date()) - 1
  currYear <- year(Sys.Date())
    
  monthLeads <- data.frame(TempMonthlyAnoms[,'LEAD'])
  
  if(day(Sys.Date()) < 15){
    monthLeads$Month <- monthLeads$LEAD + currMonth - 1
    } else {
    monthLeads$Month <- monthLeads$LEAD + currMonth
    }
  
  # leads
  monthLeads$lead1 <- monthLeads$LEAD
  monthLeads$lead2 <-  monthLeads$LEAD - 1
  monthLeads$lead3 <-  monthLeads$LEAD - 2
  
  monthLeads[monthLeads <= 0] <- NA 
  monthLeads$Month <- ifelse(monthLeads$Month > 12, monthLeads$Month - 12, monthLeads$Month)
  monthLeads <- monthLeads[1:12,]
  
  # Begin generating futures! ----------------------------------------------------------------------
  realization_SDs_temp <- rnorm(n)
  realization_SDs_ppt <- rnorm(n)
  
  for(nn in seq(n)) { # Create n years of data
    print(nn)
    
    TempMonthlyAnoms$Modifier <-  realization_SDs_temp[nn]
    PPTMonthlyAnoms$Modifier <-  realization_SDs_ppt[nn]
    
    # Step 1 - generate an anomaly for each lead and future by --------------------------------------------------
    # - A. A sd modifier for each temp and ppt will be selected for each run. This modifier is consistent for each run
    # - B. Apply the modifier to generate forecast: forecasted mean + (forecasted sd * modifier)
    # - C. Subtract climatological mean from forecasted #

     # Temp
    TempMonthlyAnoms$Prediction_F <-  as.numeric(TempMonthlyAnoms$ForecastedMEAN) +
                        (as.numeric(TempMonthlyAnoms$ForecastedSD) *  TempMonthlyAnoms$Modifier)
    TempMonthlyAnoms$Anom_F <- TempMonthlyAnoms$Prediction_F - TempMonthlyAnoms$ClimatologicalMEAN
    TempMonthlyAnoms$Anom_C <- TempMonthlyAnoms$Anom_F * (5/9)

    # PPT
    backT <- 1/PPTMonthlyAnoms$PO
    PPTMonthlyAnoms$ForecastedSD_in <- as.numeric(PPTMonthlyAnoms$ForecastedSD) ^ backT
    PPTMonthlyAnoms$Prediction_in <- as.numeric(PPTMonthlyAnoms$ForecastedMEAN ^ PPTMonthlyAnoms$PO) +
                        (as.numeric(PPTMonthlyAnoms$ForecastedSD) * PPTMonthlyAnoms$Modifier)
   
    PPTMonthlyAnoms$Prediction_in <- PPTMonthlyAnoms$Prediction_in ^ backT
    
    PPTMonthlyAnoms$Anom_CF <- PPTMonthlyAnoms$Prediction_in / PPTMonthlyAnoms$ClimatologicalMEAN
    PPTMonthlyAnoms$Anom_in <- PPTMonthlyAnoms$Prediction_in - PPTMonthlyAnoms$ClimatologicalMEAN
    PPTMonthlyAnoms$Anom_cm <- PPTMonthlyAnoms$Anom_in * 2.54
    
    PPTMonthlyAnomsAll <- rbind(PPTMonthlyAnomsAll, PPTMonthlyAnoms)# for testing purposes only
    TempMonthlyAnomsAll <- rbind(TempMonthlyAnomsAll, TempMonthlyAnoms)# for testing purposes only
    
    # Step 2 Get monthly averages -------------------------------------------------------------------------------
    yearlydat <- data.frame(matrix(nrow = 12, ncol = 3))
    names(yearlydat) <- c('tempAnom', 'pptAnom_CF', 'pptAnom_cm')
    yearlydat$MN <- as.numeric(row.names(yearlydat))
    
    for(m in c(yearlydat$MN)){ # for each month, m, in a year, nn
      
      leads <- c(t(monthLeads[monthLeads$Month == m, 3:5]))

      yearlydat[m, 1] <- mean(TempMonthlyAnoms$Anom_C[leads], na.rm = TRUE) 
      # ppt
      yearlydat[m, 2] <- mean(PPTMonthlyAnoms$Anom_CF[leads], na.rm = TRUE)
      # ppt2 .. for testing
      yearlydat[m, 3] <- mean(PPTMonthlyAnoms$Anom_cm[leads], na.rm = TRUE)/sum(!is.na(leads))
    }
    AnomSave <- rbind(AnomSave, yearlydat)
    
    # Step 3 ----------------------------------------------------------------------------------------------
    # Create future weather / integrate anomaly data into historical weather ----------------------------------------------------------
    wdata2 <- wdata[wdata$Year %in% 1980:2010, ]
    weathAnomAll <- suppressWarnings(integrateAnomalyData(wdata2, yearlydat))
    weathAnomAll <- dbW_dataframe_to_weatherData(weathAnomAll[weathAnomAll$Year %in% c(1980:2010),], round = 4)

    # Make weather data for one simulation
    ## Three years worth of data: 1) One year ago 2) Observed until today's date 3) Future data integrated with historical data (weath Anom)
    ## Run 30 times, where each time corresponds to a different year in the historical record
    
    ### year 1 - The year prior to current year's observed data
    year1 <- dbW_dataframe_to_weatherData(wdata[wdata$Year == (currYear - 1),], round = 4)
    
    # observed weather -----------------------------------------------
    thisYearObservedWData <- dbW_dataframe_to_weatherData(wdata[wdata$Year == currYear, ], round = 4)
    days <- dim(thisYearObservedWData[[1]]@data)[1]
    
    for(y in 1:30){
   #   print(y)
      ### ---------
      ### year 2 - observed data for this year until today's date and then future, weathAnom data
      ### ---------
      year2 <- thisYearObservedWData
      
      year2Fut <- weathAnomAll[[y]]
      year2Fut@year <- as.integer(currYear) # change year
      
      # add future data where appropriate
       # add to leap years data ....
      if(days == 366 & y %in% seq(1,30,4)) year2[[1]]@data[currDOY:days,] <- year2Fut@data[currDOY:days,] 
      # not a leap year but days taken from leap year...
      if(days == 366 & y %in% c(2:4,6:8,10:12,14:16,18:20,22:24,26:28,30)) year2[[1]]@data[currDOY:365,] <- year2Fut@data[currDOY:365,] 
      
      if(days == 365) year2[[1]]@data[currDOY:365,] <- year2Fut@data[currDOY:365,] #what if days = 365 & y is leap year - will need to add aextra days (if days == 365 & y %in% seq(1,30,4) )
      
      ### ---------
      ## year 3 ... forecasts that run into next year (aka 2021) and then scratch data for the rest of 2021
      ### ---------
      year3 <- year2Fut 
      if(days == 366) year3@data <- year3@data[1:365,] #if this year is 366 days, next year needs to be 365
      year3@year <- as.integer(currYear + 1)
      
      weathAnomOneSim <- c(year1, year2, year3)
      
      # run SOILWAT2 for future years ----------------------------------------------------------
      swYears_EndYear(sw_in0) <- currYear + 1
      swYears_StartYear(sw_in0) <- currYear - 1
      
      sw_out <- sw_exec(inputData = sw_in0, weatherList = weathAnomOneSim)
      
      # Grab Data I want for this run ----------------------------------------------------------
      Out1 <- getOutputs(sw_out, future = TRUE)
      Out1$run <- paste(nn, y, sep = '_')

      AllOut <- rbind(AllOut, Out1)
      
    }
      }
  
  return(list(AllOut, AnomSave, TempMonthlyAnomsAll, PPTMonthlyAnomsAll))

}

integrateAnomalyData <- function(wdata, yearlydat) {
 
  # This function alters the historical weather data so that every year in the historical record
  # now has future anomolies applied
  ## Temp -> additive
  ## PPT -> multiplicative scaled
  
  ### Step 1 - find month in wdata and merge anomalies
  wdata$MN <-  month(as.Date(strptime(paste(wdata$Year, wdata$DOY), format="%Y %j"), format="%m-%d-%Y"))
  #test <- wdata[wdata$DOY == 366,]
  wdata <- merge(wdata, yearlydat)
  
  ### Temp ------------------------------------------------------------------------------------ 
  wdata$Tmax_C <- wdata$Tmax_C + wdata$tempAnom
  wdata$Tmin_C <- wdata$Tmin_C + wdata$tempAnom
  
  ### Precip ----------------------------------------------------------------------------
  wdata$PPT_cm <- wdata$PPT_cm * wdata$pptAnom_CF
  
  #That's it?
  wdata <- wdata[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
  wdata <- setorder(wdata, Year, DOY)
  return(wdata)
   
}


