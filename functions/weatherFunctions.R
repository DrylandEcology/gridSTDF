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
  
  Nleads <- 12
  
  MonthlyAnoms <- data.frame() # for testing
  AllOut <- data.frame()

  # Determine Region from coordinates and shapefile ------------------------------------------
  CD102 <- shapefile(x = 'CD102/CD102.shp')
  points <- data.frame(x = lng, y = lat)
  
  coordinates(points) <- ~ x + y 
  proj4string(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  CDRegion <- as.numeric(over(points, CD102)$ID4)
  
  # Read in and subset anomaly data -----------------------------------------------------------
  TempAnomsWhole <- fread('CurrentAnomalyTempData.csv')
  TempAnoms <- subset(TempAnomsWhole, CD == CDRegion)

  PPTAnomsWhole <- fread('CurrentAnomalyPPTData.csv')
  PPTAnoms <- subset(PPTAnomsWhole, CD == CDRegion)

  PPTAnoms <- PPTAnoms[1:Nleads,]
  TempAnoms <- TempAnoms[1:Nleads]
  # Establish date and time info -------------------------------------------------------------
  
  # Establish current month and how the 'LEAD's relate to months
  # A LEAD of 1 relates to the forecast beginning where the currmonth + 1
  
  currMonth <- month(Sys.Date())
  currDOY <- yday(Sys.Date()) - 1
  currYear <- year(Sys.Date())
  
  monthLeads <- makeMonthLeadRelationshipTable(TempAnoms)
  
  # Prepare historical data -------------------------------------------------------------------
  ## Calculate mean daily temperature
  wdata$Tmean_C <- rowMeans(wdata[,c('Tmax_C', 'Tmin_C')])
  
  # Determine months
  wdata$Month <- month(wdata$Date)
  wdata$Date <- NULL
  
  # Aggregate to monthly values
  monthlyWdata <- wdata[,.(Tmean_C = mean(Tmean_C), PPT_cm = sum(PPT_cm)), .(Month, Year)]
  
  # Convert monthly PPT to inches 
  monthlyWdata$PPT_in <- monthlyWdata$PPT_cm / 2.54
  
  # Aggregate to moving left-aligned 3-month periods (NWS lead seasons)
  monthlyWdata$Tmean_C_rollMean <- zoo::rollmean(x = monthlyWdata[,c("Tmean_C")],
                                                 k = 3, FUN = mean, fill = NA, 
                                                 partial = TRUE, align = "left")
  monthlyWdata$PPT_in_rollSum <- zoo::rollsum(x = monthlyWdata[,c("PPT_in")],
                                                k = 3, fill = NA, 
                                                partial = TRUE, align = "left")
  # Norm
  monthlyWdata <- monthlyWdata[monthlyWdata$Year %in% 1981:2010, ]
  
  # Convert moving left-aligned 3-month periods to NWS leads
  monthLeads2 <- monthLeads[1:Nleads,]
  monthlyWdata <- merge(monthlyWdata, monthLeads2[,1:2])

  # Transform PPT_in_rollingSum using powers (PO) from NWS long-long ppt forecasts
  monthlyWdata <- merge(monthlyWdata, PPTAnoms[,c('LEAD', 'PO')], by = 'LEAD')
  monthlyWdata$PPT_PO_rollSum <- monthlyWdata$PPT_in_rollSum ^ monthlyWdata$PO
  
  # Begin generating futures! ----------------------------------------------------------------------
  # Step 1 - generate an anomaly for each lead -------------------------------------------------
  
  # Temp
  TempAnoms$ClimatologicalMEAN_Temp_C <- (TempAnoms$ClimatologicalMEAN - 32) * (5/9)
  TempAnoms$ForecastedMEAN_Temp_C <- (TempAnoms$ForecastedMEAN - 32) * (5/9)
  
  TempAnoms$ForecastedSD_Temp_C <- (TempAnoms$ForecastedSD) * (5/9)
  TempAnoms$Anom_F <- TempAnoms$ForecastedMEAN - TempAnoms$ClimatologicalMEAN
  TempAnoms$Anom_C <- TempAnoms$Anom_F * (5/9)
  
  # PPT - keep in transformed units
  backT <- 1/PPTAnoms$PO
  # convert
  PPTAnoms$ClimatatologicalMEAN_PPT_PO <- PPTAnoms$ClimatologicalMEAN ^ PPTAnoms$PO
  PPTAnoms$ClimatatologicalMEAN_PPT_cm <- PPTAnoms$ClimatologicalMEAN * 2.54
  
  PPTAnoms$ForecastedMEAN_PPT_PO <- PPTAnoms$ForecastedMEAN ^ PPTAnoms$PO
  PPTAnoms$ForecastedMEAN_PPT_cm <- PPTAnoms$ForecastedMEAN * 2.54
  
  PPTAnoms$ForecastedSD_PPT_in <- as.numeric(PPTAnoms$ForecastedSD) ^ backT
  PPTAnoms$ForecastedSD_PPT_cm <- PPTAnoms$ForecastedSD_PPT_in * 2.54
  
  # calc - anomaly NEEDS to be in transformed units
  PPTAnoms$Anom_PO <- PPTAnoms$ForecastedMEAN_PPT_PO - PPTAnoms$ClimatatologicalMEAN_PPT_PO
  PPTAnoms$Anom_cm <- PPTAnoms$ForecastedMEAN_PPT_cm - PPTAnoms$ClimatatologicalMEAN_PPT_cm
  PPTAnoms$Anom_CF <- PPTAnoms$ForecastedMEAN / PPTAnoms$ClimatologicalMEAN
  
  # fwrite(PPTAnoms, 'ExampleData/PPTAnoms.csv')
  # fwrite(TempAnoms, 'ExampleData/TempAnoms.csv')
   
  # Step 2 - n samples, multivariate sampling for each lead -------------------------------------------------
  generatedAnomData <- generateAnomalyData(monthlyWdata, TempAnoms, PPTAnoms, 
                                           leads = seq_len(Nleads), Nleads = Nleads, 
                                           n = n)
  
  #saveRDS(generatedAnomData,  'ExampleData/generatedAnomData')  
  
  for(nn in 1:n){
    print(nn)
    
    # Step 3 Get monthly averages across leads --------------------------------------------------
    OneYearAnom <- generatedAnomData[ , nn, ]
    
    yearlydat <- data.frame(matrix(nrow = 12, ncol = 3))
    names(yearlydat) <- c('tempAnom', 'pptAnom_cm', 'pptAnom_CF')
    yearlydat$Month <- as.numeric(row.names(yearlydat))
  
    for(m in c(yearlydat$Month)){ # for each month, m, in a year, nn
    
      mLeads <- c(t(monthLeads[monthLeads$Month == m, 3:5]))

      #temp
      yearlydat[m, 1] <- mean(OneYearAnom[mLeads, 'dT_C'], na.rm = TRUE) 
      
      # ppt cm
      OneYearAnom  <- cbind(OneYearAnom, Anom_cm =  OneYearAnom[,4] - PPTAnoms$ClimatatologicalMEAN_PPT_cm[1:12])
      yearlydat[m, 2] <-  mean(OneYearAnom[mLeads, 'Anom_cm'], na.rm = TRUE)/sum(!is.na(mLeads))
      
      # ppt correction factor
      yearlydat[m, 3] <-  mean(OneYearAnom[mLeads, 'PPT_CF'], na.rm = TRUE)

    }
  
    MonthlyAnoms <- rbind(MonthlyAnoms, yearlydat)
  
    #fwrite(MonthlyAnoms, 'ExampleData/MonthlyAnoms.csv')
    
    # Step 4 ----------------------------------------------------------------------------------------------
    # Create future weather / integrate anomaly data into historical weather ----------------------------------------------------------
    years <- 1981:2010
    wdata2 <- wdata[wdata$Year %in% years, ]
    weathAnomAll <- suppressWarnings(integrateAnomalyData(wdata2, yearlydat))
    
    # Make weather data for one simulation
    ## Three years worth of data: 1) One year ago 2) Observed until today's date 3) Future data integrated with historical data (weath Anom)
    ## Run 30 times, where each time corresponds to a different year in the historical record
  
    ### year 1 - The year prior to current year's observed data
    year1 <- wdata[wdata$Year == (currYear - 1), c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
    
    # observed weather -----------------------------------------------
    thisYearObservedWData <- wdata[wdata$Year == currYear, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
    days <- dim(thisYearObservedWData)[1]

    for(y in years){
   #   print(y)
      ### ---------
      ### year 2 - observed data for this year until today's date and then future, weathAnom data
      ### ---------
      year2 <- thisYearObservedWData
      
      year2Fut <- weathAnomAll[weathAnomAll$Year == y, ]
      year2Fut$Year <- as.integer(currYear) # change year
      
      # add future data where appropriate
      if(days == 366 & y %in% seq(1980, 2010, 4)) 
        year2[currDOY:days,] <- year2Fut[currDOY:days,] 
      
      # not a leap year but days taken from leap year...
      if(days == 366 & y %in% years[! years %in% seq(1980, 2010, 4)]) 
        year2[currDOY:365,] <- year2Fut[currDOY:365,] 
      
      if(days == 365) 
        year2[currDOY:365,] <- year2Fut[currDOY:365,] #what if days = 365 & y is leap year - will need to add aextra days (if days == 365 & y %in% seq(1,30,4) )
      
      ### ---------
      ## year 3 ... forecasts that run into next year (aka 2021) and then scratch data for the rest of 2021
      ### ---------
      year3 <- year2Fut 
      if(days == 366) year3 <- year3[1:365,] #if this year is 366 days, next year needs to be 365
      year3$Year <- as.integer(currYear + 1)
      
      weathAnomOneSim <- rbind(year1, year2, year3)
      weathAnomOneSim <- dbW_dataframe_to_weatherData(weathAnomOneSim, round = 4)
      
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
    
  return(list(AllOut, MonthlyAnoms))

}

generateAnomalyData <- function(monthlyWdata, TempAnoms, PPTAnoms, 
                                leads, Nleads, n = 30) {
  
  set.seed(125)
  
  # one table
  forecast_NWS <- merge(TempAnoms[,c('LEAD','ClimatologicalMEAN_Temp_C', 'ForecastedSD_Temp_C', 'Anom_C')],
                       PPTAnoms[,c('LEAD','ClimatatologicalMEAN_PPT_PO', 'ForecastedSD', 'Anom_PO')])
  
  #------ Calculate anomalies from historical data corresponding to NWS deviations
  ids <- monthlyWdata[["LEAD"]]
  meteo_anomalies_leads <- data.frame(
    Lead = ids,
    dT_C = monthlyWdata[["Tmean_C_rollMean"]] - forecast_NWS[["ClimatologicalMEAN_Temp_C"]][ids],
    dPPT_PO = monthlyWdata[["PPT_PO_rollSum"]] - forecast_NWS[["ClimatatologicalMEAN_PPT_PO"]][ids]
  )
 
  # Within-lead covariances among dT and dPPT from historical data
  cov_anomalies_leads <- by(
    data =  meteo_anomalies_leads[c("dT_C", "dPPT_PO")], # why covariance on the difference?
    INDICES = meteo_anomalies_leads["Lead"],
    FUN = cov,
    use = "na.or.complete"
  )

  #------ Generate repeated multivariate-normal random draw for each lead
  gen_anomalies_leads <- array(
    NA,
    dim = c( length(leads), n,  5),
    dimnames = list(NULL, NULL, c("dT_C", "dPPT_PO", 'PT_GenForecasted_PO', 'PPT_GenForecasted_cm', 'PPT_CF'))
  )
  
  for (k in seq_along(leads)) {
    # Put together covariances from historical meteo data and
    # variances from NWS forecasts
    kcov <- cov_anomalies_leads[[k]]
    kcov["dT_C", "dT_C"] <- t(forecast_NWS[k, "ForecastedSD_Temp_C"] ^ 2)
    kcov["dPPT_PO", "dPPT_PO"] <- t(forecast_NWS[k, "ForecastedSD"] ^ 2)
    
    # Draw multivariate normal anomalies: Sigma may not be positive definite
    tmp <- tryCatch(
        MASS::mvrnorm(
          n = n,
          mu = as.numeric( forecast_NWS[k, c("Anom_C", "Anom_PO")]), # Means as forecasted NWS anoms
         Sigma = kcov,
         empirical = TRUE
         ),
       
       error = function(e) {
         # Try & Make sigma positive definite
         kcov2 <- Matrix::nearPD(
           x = kcov,
           keepDiag = FALSE,
           maxit = 1000
         )
         
        if (kcov2[["converged"]]) {
          tmp <- MASS::mvrnorm(
            n = n,
            mu = as.numeric(forecast_NWS[k, c("Anom_C", "Anom_PO")]),  # Means as forecasted NWS anoms
            Sigma = kcov2$mat,
            empirical = TRUE)
        } else {
          print(paste('drawing independent anomalies for lead', k))
          # draw instead independent anomalies
          kcov["dPPT_PO", "dT_C"] <- kcov["dT_C", "dPPT_PO"] <- 0
          tmp <- MASS::mvrnorm(
            n = n,
            mu = as.numeric(forecast_NWS[k, c("Anom_C", "Anom_PO")]),  # Means as forecasted NWS anoms
            Sigma = kcov,
            empirical = TRUE)
        }
         return(tmp)
         }

     )
     
     gen_anomalies_leads[k, , c(1,2)] <- tmp
  }
  
  # generate forecast in transformed units
  gen_anomalies_leads[, , 'PT_GenForecasted_PO']  <- 
    gen_anomalies_leads[, , 'dPPT_PO'] + PPTAnoms$ForecastedMEAN_PPT_PO[1:12]
  
  # convert ppt forecast to cm
  backT <- 1/PPTAnoms$PO[1:12]
  gen_anomalies_leads[, , 'PPT_GenForecasted_cm'] <- ((gen_anomalies_leads[, , 'PT_GenForecasted_PO']) ^ backT) * 2.54
  
  # calculate correction factor
  gen_anomalies_leads[, , 'PPT_CF'] <- gen_anomalies_leads[, , 'PPT_GenForecasted_cm'] / PPTAnoms$ClimatatologicalMEAN_PPT_cm[1:12]
  
  
  return(gen_anomalies_leads)
  
}

integrateAnomalyData <- function(wdata, yearlydat) {
 
  # This function alters the historical weather data so that every year in the historical record
  # now has future anomolies applied
  ## Temp -> additive
  ## PPT -> multiplicative scaled
  
  ### Step 1 - find month in wdata and merge anomalies
  wdata$Month <-  month(as.Date(strptime(paste(wdata$Year, wdata$DOY), format="%Y %j"), format="%m-%d-%Y"))
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

makeMonthLeadRelationshipTable <- function(TempAnoms) {
  
  currMonth <- month(Sys.Date())
  
  monthLeads <- data.frame(TempAnoms[,'LEAD'])
  
  if(day(Sys.Date()) < 15){ # specific forecasts dates depend on schedule
    monthLeads$Month <- monthLeads$LEAD + currMonth - 1
  } else {
    monthLeads$Month <- monthLeads$LEAD + currMonth
  }
  
  # leads
  monthLeads$lead1 <- monthLeads$LEAD
  monthLeads$lead2 <-  monthLeads$LEAD - 1
  monthLeads$lead3 <-  monthLeads$LEAD - 2
  
  monthLeads[monthLeads <= 0] <- NA 
  monthLeads[monthLeads == 13] <- 1
  
  monthLeads$Month <- ifelse(monthLeads$Month > 12, monthLeads$Month - 12, monthLeads$Month)
  
  return(monthLeads)
}
