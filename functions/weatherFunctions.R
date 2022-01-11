getWeatherData <- function(lat, lng, currYear, dir) {
  
  # Make data.frame of just years and days -------------------------------------------
  wdata <- data.frame(Date = seq(from = as.Date('2021-01-01'),
                                 to = as.Date(paste0(currYear,'-12-31')), by="day"))
  wdata$Year <- year(wdata$Date)
  wdata$DOY <- yday(wdata$Date)
  wdata$Tmax_C <- wdata$Tmin_C <- wdata$PPT_cm <- 1
  
  # Get info from netcdfs ....
  # https://www.northwestknowledge.net/metdata/data/
  files <- list.files(dir, full.names = TRUE)
  
  # Tmax_C
  tmmxfiles <- grep('tmmx_', files, value = TRUE)
  for(f in 1:length(tmmxfiles)){
    year <- as.numeric(substr(tmmxfiles[f],  nchar(tmmxfiles[f]) - 6, nchar(tmmxfiles[f]) -3))
    
    nc <- suppressWarnings(brick(tmmxfiles[f], varname = 'air_temperature'))
    vals <- extract(nc, matrix(c(as.numeric(lng), as.numeric(lat)), ncol = 2))[1,]
    
    # determine "data of last weather" here
    if(year == currYear){
      lastWeatherDate <- as.Date(length(vals), origin = paste0(currYear,"-01-01"))
    }
    
    wdata[wdata$Year == year, 'Tmax_C'][1:length(vals)] <- vals
  }
  
  # Tmin_C
  tmmnfiles <- grep('tmmn_', files, value = TRUE)
  for(f in 1:length(tmmnfiles)){
    year <- as.numeric(substr(tmmnfiles[f],  nchar(tmmnfiles[f]) - 6, nchar(tmmnfiles[f]) -3))
    
    nc <- suppressWarnings(brick(tmmnfiles[f], varname = 'air_temperature'))
    vals <- extract(nc, matrix(c(as.numeric(lng), as.numeric(lat)), ncol = 2))[1,]
    wdata[wdata$Year == year, 'Tmin_C'][1:length(vals)] <- vals
  }
  
  #PPT_cm
  prfiles <- grep('pr_', files, value = TRUE)
  for(f in 1:length(prfiles)){
    year <- as.numeric(substr(prfiles[f],  nchar(prfiles[f]) - 6, nchar(prfiles[f]) -3))
    
    nc <- suppressWarnings(brick(prfiles[f], varname = 'precipitation_amount'))
    vals <- extract(nc, matrix(c(as.numeric(lng), as.numeric(lat)), ncol = 2))[1,]
    wdata[wdata$Year == year, 'PPT_cm'][1:length(vals)] <- vals
  }
  
  # if there isn't 365 days in each year ... na.locf for temp and put 0 for ppt?
  # fill in missing with weather generator when running SOILWAT?
  wdata$Tmax_C <- zoo::na.locf(wdata$Tmax_C)
  wdata$Tmin_C <- zoo::na.locf(wdata$Tmin_C)
  wdata[is.na(wdata$PPT_cm), 'PPT_cm'] <- 0
  
  # convert
  wdata$Tmax_C <- wdata$Tmax_C - 273.15
  wdata$Tmin_C <- wdata$Tmin_C - 273.15
  wdata$PPT_cm <- wdata$PPT_cm /10
  return(list(wdata, lastWeatherDate))
}

runFutureSWwithAnomalies <- function(sw_in0, wdata, SoilsDF,  
                                     TempAnoms, PPTAnoms, 
                                     Nleads, n,
                                     currDOY, currMonth, currYear, currDate){
  
  # Prepare historical data ----------------------------------------------------
  wdata_dt <- rSOILWAT2::dbW_weatherData_to_dataframe(wdata)
  Month <- lubridate::month(strptime(paste0(as.character(wdata_dt[,'Year']), as.character(wdata_dt[,'DOY'])),format="%Y %j"))
  wdata_dt <- cbind(wdata_dt, Month)
  
  ## Calculate mean daily temperature
  wdata_dt <- cbind(wdata_dt, 'Tmean_C' = rowMeans(wdata_dt[,c('Tmax_C', 'Tmin_C')]))
  
  # Aggregate to monthly values
  wdata_dt <- setDT(as.data.frame(wdata_dt))
  monthlyWdata <- wdata_dt[,.(Tmean_C = mean(Tmean_C), 
                              PPT_cm = sum(PPT_cm)), .(Month, Year)]
  
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
  monthlyWdata <- merge(monthlyWdata, monthLeads[,1:2])
  
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
  
  # Anomalies NEEDS to be in transformed units for MV sampling
  PPTAnoms$Anom_PO <- PPTAnoms$ForecastedMEAN_PPT_PO - PPTAnoms$ClimatatologicalMEAN_PPT_PO
  
  # Anomalies for comparison
  PPTAnoms$Anom_cm <- PPTAnoms$ForecastedMEAN_PPT_cm - PPTAnoms$ClimatatologicalMEAN_PPT_cm
  PPTAnoms$Anom_CF <- PPTAnoms$ForecastedMEAN / PPTAnoms$ClimatologicalMEAN
  
  # fwrite(PPTAnoms, 'ExampleData/PPTAnoms.csv')
  # fwrite(TempAnoms, 'ExampleData/TempAnoms.csv')
  
  # Step 2 - n samples, multivariate sampling for each lead -------------------------------------------------
  generatedAnomData <- generateAnomalyData(monthlyWdata, TempAnoms, PPTAnoms,
                                           leads = seq_len(Nleads), Nleads = Nleads,
                                           n = 30)
  #saveRDS(generatedAnomData,  'ExampleData/generatedAnomData')
  
  # Step 2.1 - Correction factor to the correction factor based on mean -------------------------------
  
  ### Temperature
  tempGenAnoms <- data.table(generatedAnomData [, , "dT_C"])
  tempGenAnoms$LEAD <- row.names(tempGenAnoms)
  tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
  tempGenAnoms <- tempGenAnoms[,.(MVGenMean = mean(value)),.(LEAD)]
  
  TempBiasCF <- TempAnoms$Anom_C - tempGenAnoms$MVGenMean
  #print(cbind(NWSAnom  = TempAnoms$Anom_C , MVAnom = tempGenAnoms$MVGenMean,  TempBiasCF))
  
  generatedAnomData [, , "dT_C"] <- generatedAnomData [, , "dT_C"] + TempBiasCF
  
  ### Precipitation
  pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
  pptGenAnoms$LEAD <- row.names(pptGenAnoms)
  pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
  pptGenAnomsMean <- pptGenAnoms[,.(MVGenMean = mean(value)),.(LEAD)]
  
  PPTBiasCF <- PPTAnoms$Anom_CF / pptGenAnomsMean$MVGenMean
  #print(cbind(NWSAnom  = PPTAnoms$Anom_CF, MVAnom = pptGenAnomsMean$MVGenMean,  PPTBiasCF))
  
  generatedAnomData [, , "PPT_CF"] <- generatedAnomData [, , "PPT_CF"] * PPTBiasCF
  
  ##### ------ Recalculate PPT_GenForecasted_cm
  ## ----- need for figures but not used anywhere in analysis
  generatedAnomData[, , 'PPT_GenForecasted_cm'] <-
    generatedAnomData [, , "PPT_CF"] * PPTAnoms$ClimatatologicalMEAN_PPT_cm
  
  #saveRDS(generatedAnomData,  'Git/shorttermdroughtforecaster/ExampleData/generatedAnomData_BiasCorrected')
  AllOut1 <- data.frame()
  for(nn in 1:n){
    print(nn)
    
    # Step 3 Get monthly averages across leads --------------------------------------------------
    OneYearAnom <- generatedAnomData[ , nn, ]
    OneYearAnom  <- cbind(OneYearAnom, Climatological_MEAN_cm = PPTAnoms$ClimatatologicalMEAN_PPT_cm[1:12])
    OneYearAnom <- cbind(OneYearAnom, Anom_cm = OneYearAnom[,4] - OneYearAnom[,6])
    
    yearlydat <- matrix(nrow = 12, ncol = 3)
    colnames(yearlydat) <- c('tempAnom', 'pptAnom_cm', 'pptAnom_CF')
    yearlydat <- cbind(yearlydat, 'Month' = 1:12)
    
    for(m in seq(12)){ # for each month, m, in a year, nn
      
      mLeads <- c(t(monthLeads[monthLeads$Month == m, 3:5]))
      
      #temp
      yearlydat[m, 1] <- mean(OneYearAnom[mLeads, 'dT_C'], na.rm = TRUE)
      
      # ppt cm
      yearlydat[m, 2] <-  mean(OneYearAnom[mLeads, 'Anom_cm'], na.rm = TRUE)/3
      
      # ppt correction factor
      yearlydat[m, 3] <-  mean(OneYearAnom[mLeads, 'PPT_CF'], na.rm = TRUE)
      
    }
    
    
    #MonthlyAnoms <- rbind(MonthlyAnoms, yearlydat)
    #}
    #fwrite(MonthlyAnoms, 'ExampleData/MonthlyAnoms.csv')
    
    # Step 4 ----------------------------------------------------------------------------------------------
    # Create future weather / integrate anomaly data into historical weather ----------------------------------------------------------
    years <- 1991:2021
    wdata2 <- wdata_dt[wdata_dt$Year %in% years, ]
    weathAnomAll <- integrateAnomalyData(wdata2, yearlydat)
    
    # Make weather data for one simulation
    ## Three years worth of data:
    # 1) One year ago
    # 2) Observed until today's date
    # 3) Future data integrated with historical data (weath Anom)
    
    ### year 1 - The year prior to current year's observed data
    year1 <- wdata_dt[wdata_dt$Year == currYear - 1, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
    
    # observed weather -----------------------------------------------
    thisYearObservedWData <- wdata_dt[wdata_dt$Year == currYear, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
    thisYearObservedWData <- makeDateMonthDat(thisYearObservedWData, 'DOY')
    
    for(y in 1991:2020) {
      #print(y)
      weathAnomOneSim <- makeWeathOneSim(y, year1, thisYearObservedWData, weathAnomAll,
                                         currDOY, currYear)
      # run SOILWAT2 for future years ----------------------------------------------------------
      weathAnomOneSim <- dbW_dataframe_to_weatherData(weathAnomOneSim, round = 4)
      
      swYears_EndYear(sw_in0) <- currYear + 1
      swYears_StartYear(sw_in0) <- currYear - 1
      
      sw_out <- sw_exec(inputData = sw_in0, weatherList = weathAnomOneSim, quiet = TRUE)
      
      # # Grab Data I want for this run ----------------------------------------------------------
      Out1 <- getOutputs(sw_out, sw_in0, SoilsDF, 
                         calc_EcoVars = FALSE, 
                         TimePeriod = 'Future',
                          currYear, currDate)
      
  
      names(Out1[1]) <-  paste(nn, y, sep = '_')
      AllOut1 <- c(AllOut1, Out1)
      
      # Shriver_Out <- rbind(Shriver_Out, cbind(Out1[[2]], run = paste(nn, y, sep = '_')))
      # GISSM_Out <- rbind(GISSM_Out, cbind(Out1[[3]], run = paste(nn, y, sep = '_')))
      # OConnor_Out <- rbind(OConnor_Out, Out1[[4]])
    }
    
  } 
  return(AllOut1)
}

  #return(Out1)
  #return(AllOut1)
  #return(list(AllOut1, Shriver_Out, GISSM_Out, OConnor_Out,
  #            MonthlyAnoms))



generateAnomalyData <- function(monthlyWdata, TempAnoms, PPTAnoms,
                                leads, Nleads, n = 30) {

  set.seed(NULL)

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
    # write message that confirms that mvrnorm worked for each lead
    tmp <- tryCatch(
        MASS::mvrnorm(
          n = n,
          mu = as.numeric( forecast_NWS[k, c("Anom_C", "Anom_PO")]), # Means as forecasted NWS anoms
         Sigma = kcov,
         empirical = TRUE
         ),

       error = function(e) {
         # Try & Make sigma positive definite
         message(paste('Attempting to make lead', k, 'positive definite'))
         print(kcov)

         kcov2 <- Matrix::nearPD(
           x = kcov,
           keepDiag = FALSE,
           maxit = 1000
         )
        if (kcov2[["converged"]]) {

          message(paste('kcov for lead', k, 'successfuly converged'))
          print(kcov2$mat)

          tmp <- MASS::mvrnorm(
            n = n,
            mu = as.numeric(forecast_NWS[k, c("Anom_C", "Anom_PO")]),  # Means as forecasted NWS anoms
            Sigma = kcov2$mat,
            empirical = TRUE)
        } else {
          message(paste('drawing independent anomalies for lead', k))
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
    gen_anomalies_leads[, , 'dPPT_PO'] + PPTAnoms$ClimatatologicalMEAN_PPT_PO[1:12]

  # convert ppt forecast to cm
  backT <- 1/PPTAnoms$PO[1:12]
  gen_anomalies_leads[, , 'PPT_GenForecasted_cm'] <- ((gen_anomalies_leads[, , 'PT_GenForecasted_PO']) ^ backT) * 2.54

  # calculate correction factor
  gen_anomalies_leads[, , 'PPT_CF'] <-  gen_anomalies_leads[, , 'PPT_GenForecasted_cm'] / PPTAnoms$ClimatatologicalMEAN_PPT_cm[1:12]


  return(gen_anomalies_leads)

}

integrateAnomalyData <- function(wdata, yearlydat) {

  # This function alters the historical weather data so that every year in the historical record
  # now has future anomolies applied
  ## Temp -> additive
  ## PPT -> multiplicative scaled

  ### Step 1 - find month in wdata and merge anomalies
 # wdata$Month <-  month(as.Date(strptime(paste(wdata$Year, wdata$DOY), format="%Y %j"), format="%m-%d-%Y"))
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
