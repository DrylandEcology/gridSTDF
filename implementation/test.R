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
    weathAnomAll <- suppressWarnings(integrateAnomalyData(wdata2, yearlydat))
    
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
      #Out1 <- getOutputs(sw_out, sw_in0, SoilsDF, 
      #                   calc_EcoVars = FALSE, 
      #                   TimePeriod = 'Future',
      #                   currYear, currDate)
      #profvis(getOutputs(sw_out, sw_in0, calc_EcoVars = FALSE, TimePeriod = 'Future',
      #           currYear, currDate))
      # 
      #names(Out1[1]) <-  paste(nn, y, sep = '_')
      #AllOut1 <- c(AllOut1, Out1)
      
      # Shriver_Out <- rbind(Shriver_Out, cbind(Out1[[2]], run = paste(nn, y, sep = '_')))
      # GISSM_Out <- rbind(GISSM_Out, cbind(Out1[[3]], run = paste(nn, y, sep = '_')))
      # OConnor_Out <- rbind(OConnor_Out, Out1[[4]])
    }
    
  } 
  
}
