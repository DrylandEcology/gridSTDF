getOutputs <- function(sw_out, sw_in, calc_Shriver = TRUE, calc_GISSM = TRUE) {
  
  # Temp and Precip
  Temp1 <- data.table(sw_out@TEMP@Day)
  Temp1 <- Temp1[, c('Year', 'Day', 'avg_C')]
 
  PPT1 <-  data.table(sw_out@PRECIP@Day)
  PPT1 <- PPT1[, c('Year', 'Day', 'ppt')]

  # VWC ---------------------------------------------------
  # Step 1 - Get weighted mean across depths for VWC
  VWC1 <-  data.table(sw_out@VWCMATRIC@Day)
  VWC1 <- melt.data.table(VWC1, id.vars = c('Year', 'Day'))

  # Get EcoVars ----------------------------------------------------------------
 
  # Shriver 2018  Vars
  if(calc_Shriver) {
    Shriver2018Vars <- getShriver2018Vars(Temp1, VWC1)
  }
  
  # GISSM Vars
  if(calc_GISSM) {
    GISSM_1 <- calc_GISSM(
      x = sw_out,
      soillayer_depths_cm = rSOILWAT2::swSoils_Layers(sw_in)[, 1],
      site_latitude = rSOILWAT2::swSite_IntrinsicSiteParams(sw_in)[["Latitude"]],
      has_soil_temperature =
        rSOILWAT2::swSite_SoilTemperatureFlag(sw_in) &&
        !rSOILWAT2::has_soilTemp_failed()
    )
  }
  # Format VWC  -------------------------------------------
  #VWC1 <- VWC1[variable != 'Lyr_1', ]
  
  VWC1 <-  merge(VWC1, SoilsDF)#, by = 'variable') 
  VWC1 <- setDT(VWC1)[,.(VWC = weighted.mean(value, width)),
                      .(Year, Day, Depth)]
  VWC1$Depth <- paste0('VWC.', VWC1$Depth)
  VWC1 <- dcast(VWC1, Year +  Day ~ Depth, value.var = 'VWC')
  
  # Join up
  Data <- merge(Temp1, PPT1, by = c('Year', 'Day'))
  Data <- merge(Data, VWC1,  by = c('Year', 'Day'))
  
  # test - eliminate leap year dat ----------------------
  # step one - make data
  #Data$Date <- makeDateMonthDat(Data, 'Day')
  #Data <- Data[Data$Date != 02-29, ]

  if(!calc_GISSM && !calc_Shriver) return(list(Data))
  if(!calc_GISSM && calc_Shriver) return(list(Data, Shriver2018Vars))
  if(calc_GISSM  && calc_Shriver) return(list(Data, Shriver2018Vars, data.table(GISSM_1[[1]])))
  
}

formatOutputsFuture <- function(AllOut, SoilsDF) {
  
  # Get means for each day in each year
  AllOut$run_Year <- sapply(strsplit(AllOut$run, '_'), '[', 2)
  AllOut <- makeDateMonthDat(AllOut, 'Day')
  AllOut$Day <- AllOut$run <- NULL
  
  AllOut2 <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))), .(run_Year, Year, Date)] # mean for every sim. year

  # Get rolling means and sums per simulation
  AllOut2 <- setorder(AllOut2, run_Year, Year, Date)
  AllOut2 <- getRolling(AllOut2)
  
  # Make real dates
  AllOut2$Date <- as.Date(paste(AllOut2$Year, AllOut2$Date, sep = '-'))
  
  # Get quantiles of rolling
  AllOut2$run_Year <- AllOut2$Year <- NULL 

  AnomRunStats <- setnames(setDT(AllOut2)[, sapply(.SD, function(x) list(med=median(x), 
                                                                        x10=quantile(x, .1, na.rm = TRUE), 
                                                                        x90 = quantile(x, .9, na.rm = TRUE))),
                                         .(Date)],
                           c('Date', sapply(names(AllOut2)[-c(1)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!
  
  
  # Convert VWC to SWP -----------------------------------------------------------
  AnomRunStats <- getSWP(AnomRunStats, SoilsDF)
  
  AnomRunStats <- AnomRunStats[AnomRunStats$Date > Sys.Date() - 183, ] # 6 month lead ins 
  AnomRunStats <- AnomRunStats[AnomRunStats$Date <  Sys.Date() + 396, ] # 6 month lead ins 
  
  AnomRunStats$Time <- ifelse(AnomRunStats$Date < Sys.Date(), 'Observed', 'Future')
  
  return(AnomRunStats)
  
}
  
getRolling <- function(Data2){
  # get rolling sums and means for ppt and temps

  Data2$ppt_rollsum <- rollapply(Data2$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
  Data2$avgC_rollmean <- runmean(Data2$avg_C,  k = 30, endrule = 'mean', align = 'center')#
  Data2$VWC.Shallow_rollmean <- runmean(Data2$VWC.Shallow,  k = 30, endrule = 'mean', align = 'center')
  Data2$VWC.Intermediate_rollmean <- runmean(Data2$VWC.Intermediate,  k = 30, endrule = 'mean', align = 'center')
  Data2$VWC.Deep_rollmean <- runmean(Data2$VWC.Deep,  k = 30, endrule = 'mean', align = 'center')
  
  return(Data2)
}

getSWP <- function(DF, Soils) {

  SoilsDF_Avg <- setDT(Soils)[, .(clay = weighted.mean(clay_frac, width),
                                    sand = weighted.mean(sand_frac, width)), .(Depth)]
  
  Depths <- unique(SoilsDF_Avg$Depth)

  for(i in 1:length(Depths)) {
  
    SWPDF <-  DF[, .SD, .SDcols = names(DF) %like% Depths[i]]
    
    SWPDF <- SWPDF[, lapply(.SD, VWCtoSWP, 
                            sand = SoilsDF_Avg[Depth == Depths[i], sand], 
                            clay = SoilsDF_Avg[Depth == Depths[i], clay])]
    
    names(SWPDF) <- gsub('VWC', 'SWP', names(SWPDF))
    
    DF <- cbind(DF, SWPDF)
    
  }
  
  return(DF)

}

getShriver2018Vars <- function(TempDF, VWCDF) {

  # these condition are for the year AFTER seeding
  # planted in fall 2018  - then need 2019 VWC and Temp data
  # fall 2019 -2020 data
  # fall 2020 - 2021 data

  Temp_1_250 <- TempDF[Day %in% 1:250, .(Temp_mean = mean(avg_C)), .(Year)]
  VWC_top_70_100 <- VWCDF[Day %in% 70:100 & variable == 'Lyr_1',
                          .(VWC_mean = mean(value)), .(Year)]

  vars <- merge(Temp_1_250, VWC_top_70_100,  by = c('Year'))
  vars$PlantedinYear <- vars$Year - 1
    
  return(vars)
  
} 

formatShriver2018 <- function(Hist_Shriver2018, Future_Shriver2018, currYear) {
  
  Hist_Shriver2018$TP <- 'Historical'
  
  Future_Shriver2018$run_year <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 2)
  Future_Shriver2018$run_sim <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 1)
  Future_Shriver2018 <- Future_Shriver2018[Year %in% (currYear-1):currYear] 
  Future_ShriverMeds <- Future_Shriver2018[, .(Prob = median(Prob)), .(run_sim, Year)]
  Future_ShriverMeds$TP <- as.character(Future_ShriverMeds$Year)
  
  return(bind(Hist_Shriver2018,Future_ShriverMeds))
}
  
formatGISSM <- function(Hist_GISSM, Future_GISSM) {
  
  Hist_GISSM$TP <- 'Historical'
  
  Future_GISSM$run_year <- sapply(strsplit(Future_GISSM$run, '_'), '[', 2)
  Future_GISSM$run_sim <- sapply(strsplit(Future_GISSM$run, '_'), '[', 1)
  Future_GISSM <- Future_GISSM[, .(Prob = mean(SeedlingSurvival_1stSeason)), .(run_sim, ryear)]
  Future_GISSM$TP <- as.character(Future_GISSM$ryear)
  
  return(bind(Hist_GISSM,Future_GISSM))
}

