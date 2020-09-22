#' get outputs from SOILWAT objects
#' 
#'  @param sw_out SOILWAT2 output object
#'  @param sw_in SOILWAT2 input object
#'  @param SoilsDF a data.frame describing soils attributes, including depth grouping
#'  @param calc_Shriver logical. Do you want to calculate results from the Shriver 2018 model
#'  @param calc_GISSM logical. Do you want to calculate results form GISSM model.
#'  
#'  @return A list of data.frames.
getOutputs <- function(sw_out, sw_in, SoilsDF, calc_Shriver = TRUE, calc_GISSM = TRUE) {

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
    GISSM_1 <- suppressWarnings(calc_GISSM(
      x = sw_out,
      soillayer_depths_cm = rSOILWAT2::swSoils_Layers(sw_in)[, 1],
      site_latitude = rSOILWAT2::swSite_IntrinsicSiteParams(sw_in)[["Latitude"]],
      has_soil_temperature =
        rSOILWAT2::swSite_SoilTemperatureFlag(sw_in) &&
        !rSOILWAT2::has_soilTemp_failed(),

    ))
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

  if(!calc_GISSM && !calc_Shriver) return(list(Data))
  if(!calc_GISSM && calc_Shriver) return(list(Data, Shriver2018Vars))
  if(calc_GISSM  && calc_Shriver) return(list(Data, Shriver2018Vars, data.table(GISSM_1[[1]])))

}

#' Calculates historical climatology based on year range
#' 
#' @param dataset A data.frame containing daily climate data
#' @param yearBegin numeric. Begin year of climatological range
#' @param yearEnd numeric. End or year of climatological range.
#' @param SoilsDF data.frame contains soils information include texture and depth.
#' 
#' @return a data.frame containing climatology (10th, 50th, 90th quantiles)

getHistoricalClimatology <- function(dataset, yearBegin, yearEnd, SoilsDF) {
  
  HistData_Norm_Stats <- dataset[dataset$Year %in% yearBegin:yearEnd, ] # historical normal
  HistData_Norm_Stats <- makeDateMonthDat(HistData_Norm_Stats, 'Day')     # Get date without the year
  HistData_Norm_Stats$Year <- HistData_Norm_Stats$Day <- NULL
  HistData_Norm_Stats <- setnames(setDT(HistData_Norm_Stats)[ ,sapply(.SD, function(x) list(med=median(x),
                                                                                            x10=quantile(x, .1, na.rm = TRUE),
                                                                                            x90 = quantile(x, .9, na.rm = TRUE))),
                                                              .(Date)],
                                  c('Date', sapply(names(HistData_Norm_Stats)[-c(11)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!
  HistData_Norm_Stats <- getSWP(HistData_Norm_Stats, SoilsDF)
  HistData_Norm_Stats <- setorder(HistData_Norm_Stats, Date)
  
  return(HistData_Norm_Stats)
}

#' Calculates monthly means of variables
#' 
#' @param AllOut data.frame containging daily climate data
#' @param SoilsDF data.frame contains soils information include texture and depth.
#' @param TP character. Either historical or future#' @param yearBegin numeric. Begin year of climatological range
#' @param yearEnd numeric. End or year of climatological range.
#' 
#' @return a data.frame containing monthly quantiles (10th, 50th, 90th) for
#' climate and soil moisture variables
#' 
formatOutputs_Monthlys <- function(AllOut, SoilsDF, TP, yearBegin, yearEnd) {
  
  # Subset Historicall data
  if(TP == 'historical') AllOut <- AllOut[AllOut$Year %in% yearBegin:yearEnd, ] 
  
  # Make Month dates -----------------------------------------------------------
  if(TP == 'future')  AllOut$run <- sapply(strsplit(AllOut$run, '_'), '[', 2)
  AllOut$Month <-  format(as.Date(strptime(paste(AllOut$Year, AllOut$Day), format="%Y %j")), format="%m")
  
  if(TP == 'future') {
    AllOut$Date <- as.Date(paste(AllOut$Year, AllOut$Month, '15', sep = '-'))
  } else{
    Date <- paste(AllOut$Month, '15', sep = '-')
    AllOut <- cbind(Date, AllOut)
  }
  
  AllOut$Month <-  AllOut$Day <- NULL
  
  # mean for every sim. year month ---------------------------------------------
  if(TP == 'future') {
    AllOutMean <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))),
                                .(run, Year, Date),
                                .SDcols = c('avg_C', 'VWC.Deep', 'VWC.Intermediate', 
                                            'VWC.Shallow')]
    AllOutSum <- setDT(AllOut)[, .(ppt.sum = sum(ppt)), .(run, Year, Date)] 
    
    AllOut <- merge(AllOutMean, AllOutSum)
    AllOut$run <-  NULL 
  } else{
    AllOutMean <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))),
                                .(Year, Date),
                                .SDcols = c('avg_C', 'VWC.Deep', 'VWC.Intermediate', 
                                            'VWC.Shallow')]
    AllOutSum <- setDT(AllOut)[, .(ppt.sum = sum(ppt)), .(Year, Date)] 
    
    AllOut <- merge(AllOutMean, AllOutSum)
    
  }
  
  AllOut$Year <- NULL
  
  # Get median, 10th, and 90th for each month
  AnomRunStats <- setnames(setDT(AllOut)[, sapply(.SD, function(x) list(med=median(x), 
                                                                        x10=quantile(x, .1, na.rm = TRUE), 
                                                                        x90 = quantile(x, .9, na.rm = TRUE))),
                                         .(Date)],
                           c('Date', sapply(names(AllOut)[-c(1)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!
  
  
  # Convert VWC to SWP -----------------------------------------------------------
  AnomRunStats <- getSWP(AnomRunStats, SoilsDF)
  
  # Subset -----------------------------------------------------------------------
  if(TP == 'future') {
    AnomRunStats <- AnomRunStats[AnomRunStats$Date > Sys.Date() - 183, ] # 6 month lead ins 
    AnomRunStats <- AnomRunStats[AnomRunStats$Date <  Sys.Date() + 396, ] #13 months in the future
  }
  
  return(AnomRunStats)
  
}

#' Format and return outputs from future SOILWAT2 runs
#' 
#' @param AllOut data.frame containing results from all future simulations.
#' @param SoilsDF data.frame contains soils information include texture and depth.
#'
#' @return data.frame containing formating (quantiles of rolling means) of future
#' simulation data
#'  
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

  return(AnomRunStats)

}

#' A function to calculate 30 days rolling mean or sums for multiple variables
#' 
#' @param Data2. A data.frame containing daily site-specific results of temperature,
#' precipitation, and volumetric water content.
#' 
#' @return A data.frame.
#' 
getRolling <- function(Data2){
  # get rolling sums and means for ppt and temps

  Data2$ppt_rollsum <- rollapply(Data2$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
  Data2$avgC_rollmean <- runmean(Data2$avg_C,  k = 30, endrule = 'mean', align = 'center')#
  Data2$VWC.Shallow_rollmean <- runmean(Data2$VWC.Shallow,  k = 30, endrule = 'mean', align = 'center')
  Data2$VWC.Intermediate_rollmean <- runmean(Data2$VWC.Intermediate,  k = 30, endrule = 'mean', align = 'center')
  Data2$VWC.Deep_rollmean <- runmean(Data2$VWC.Deep,  k = 30, endrule = 'mean', align = 'center')

  return(Data2)
}

#' calculates SWP based on VWC and soil texture
#' 
#' @param DF A data.frame with VWC vaues
#' @param SoilsDF data.frame contains soils information include texture and depth.
#' 
#' @return The originall data.frame with additional SWP columns (1 SWP column per
#' VWC column)

getSWP <- function(DF, Soils) {

  SoilsDF_Avg <- setDT(Soils)[, .(clay = weighted.mean(clay_frac, width),
                                    sand = weighted.mean(sand_frac, width)), .(Depth)]

  Depths <- unique(SoilsDF_Avg$Depth)

  for(i in 1:length(Depths)) {

    SWPDF <-  DF[, .SD, .SDcols = names(DF) %like% Depths[i]]

    SWPDF <- SWPDF[, lapply(.SD, rSOILWAT2::VWCtoSWP,
                            sand = SoilsDF_Avg[Depth == Depths[i], sand],
                            clay = SoilsDF_Avg[Depth == Depths[i], clay])]

    names(SWPDF) <- gsub('VWC', 'SWP', names(SWPDF))

    DF <- cbind(DF, SWPDF)

  }

  return(DF)

}

#' Organizes variables necessary for Shriver 2018 model prediction
#' 
#' @param TempDF a data.frame containing daily temperature data
#' @param VWCDF a data.frame containing daily temperature data
#' 
#' @return data.frame

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

#' Calculates monthly deltas between datasets, approximates daily values, and
#' formats data
#' 
#' @param HistData1 A data.frame containing historical climatologies.
#' @param HistDataMonthly A data.frame containing historical monthly means.
#' @param FutureData1 A data.frame containing future climatologies.
#' @param FutureDataMonthly A data.frame containing future monthly means.
#' @param Var character. Name of variable which deltas are being calculated for
#' @param currDate date. Today's date.
#' 
#' @return data.frame with properly formatted outputs.

calcDeltasApproxAndFormat <- function(HistData1, HistDataMonthly,
                                      FutureData1, FutureDataMonthly, 
                                      Var, currDate) {
  
  # -----------------------------------------------------------------------------
  # Step 1 - Join Dailys from before today's data (RecentPast) to reflect actual patterns (no approx / monthlys)
  # -----------------------------------------------------------------------------
  
  #### this includes calculating deltas for this first part of the year
  indx <- grep( paste0(Var, '_roll'), names(HistData1))
  Hist <- as.data.frame(HistData1)[,c(1, indx)]
  names(Hist)[2:4] <- paste0('Hist.', names(Hist)[2:4])
  
  indx <- grep( paste0(Var, '_roll'), names(FutureData1))
  indx2 <- grep( paste0(Var, '.mean.med'), names(FutureData1))
  Fut <-  as.data.frame(FutureData1)[,c(1, indx2, indx)]
  names(Fut)[2:3] <- paste0('RecentPast.', names(Fut)[2:3])
  Fut$Date <- as.Date(Fut$Date)
  
  PastDailys <- merge(Hist, Fut) 
  
  #  Calc deltas ---------------------------------------------------------------------
  # Differences between future and historical
  PastDailys[,paste0('RecentPast.',Var,'.Diffs.Med')] <- PastDailys[, 6] - PastDailys[,2]
  #PastDailys[,paste0('RecentPast.',Var,'.Diffs.10')] <- PastDailys[, 7] - PastDailys[,2]
  #PastDailys[,paste0('RecentPast.',Var,'.Diffs.90')] <- PastDailys[, 8] - PastDailys[,2]
  
  # -----------------------------------------------------------------------------
  # Step 2 - Join Monthly Means and to calc diffs (no approx yet just monthlys)
  # -----------------------------------------------------------------------------
  indx <- grep(paste0(Var), names(HistDataMonthly))
  Hist2 <- as.data.frame(HistDataMonthly)[, c(1, indx)]
  names(Hist2)[2:4] <- paste0('Hist.', names(Hist2)[2:4])
  
  indx <- grep( paste0(Var), names(FutureDataMonthly))
  Fut2 <- as.data.frame(FutureDataMonthly)[, c(1, indx)]
  names(Fut2)[2:4] <- paste0('NearFut.', names(Fut2)[2:4])
  
  FutMonths <- merge(Hist2, Fut2) 
  
  #  Calc deltas ---------------------------------------------------------------------
  
  # Differences between future and historical
  FutMonths[,paste0('NearFut.',Var,'.Diffs.Med')] <- FutMonths[, 5] - FutMonths[,2]
  FutMonths[,paste0('NearFut.',Var,'.Diffs.10')] <- FutMonths[, 6] - FutMonths[,2]
  FutMonths[,paste0('NearFut.',Var,'.Diffs.90')] <- FutMonths[, 7] - FutMonths[,2]
  
  # -----------------------------------------------------------------------------
  # Step 3 - APPROX
  # -----------------------------------------------------------------------------
  
  # Create empty daily data frame for approximating across
  Fut_Daily_Approx <- data.frame(Date = as.Date(as.Date(currDate - 31):as.Date(currDate + 365)))
  Fut_Daily_Approx <- plyr::join(Fut_Daily_Approx, FutMonths)
  
  yy <- dim(Fut_Daily_Approx)[2]
  
  for(i in 2:yy) {
    vector <- FutMonths[[i]]
    new <- approx(FutMonths$Date, vector, xout = Fut_Daily_Approx$Date,
                  na.rm = TRUE, rule = 2)$y
    
    Fut_Daily_Approx[[i]] <- new
  }
  
  Fut_Daily_Approx <- Fut_Daily_Approx[Fut_Daily_Approx$Date  > (currDate), ]
  # -----------------------------------------------------------------------------
  # Step 4 - Format and Combine per variable
  # -----------------------------------------------------------------------------
  # From PastDailys DF -------------------------------------------------------
  ## Hist.Var_rollmean.med - elim
  ## Hist.Var_rollmean.10 - elim
  ## Hist.Var_rollmean.90 - elim
  ## RecentPast.Var.mean.med = past dailys
  ## RecentPast.Var_rollmean.med = thick dailys
  ## RecentPast.Var_C.Diffs.Med - Panel two, left of purple line
  
  indx <- grep('Hist.', names(PastDailys))
  PastDailys[,indx] <- NULL
  PastDailys[PastDailys$Date > (currDate-15), 3:4] <- NA
  
  # From Fut_Daily_Approx -------------------------------------------------
  ## Hist.avg_C.mean.med - elim
  ## Hist.avg_C.mean.10 - elim
  ## Hist.avg_C.mean.90 - elim
  ## "Fut.avg_C.mean.med" - Panel 1, blue      
  ## "Fut.avg_C.mean.10" - Panel 1
  ## "NearFut.avg_C.Diffs.Med" - Panel 2
  ## "NearFut.avg_C.Diffs.10" - Panel 2
  ## "NearFut.avg_C.Diffs.90" - Panel 2
  
  indx <- grep('Hist.', names(Fut_Daily_Approx))
  Fut_Daily_Approx[,indx] <- NULL
  Fut_Daily_Approx[Fut_Daily_Approx$Date < (currDate + 15), 2:7] <- NA
  
  AllDailys <- data.frame(Date = as.Date(as.Date(currDate - 183):as.Date(currDate + 365)))
  AllDailys <- plyr::join(AllDailys, PastDailys)
  AllDailys <- plyr::join(AllDailys, Fut_Daily_Approx)
  
  return(AllDailys)
  
}


#' Organizes all Shriver output for passing along API
#' 
#' @param Hist_Shriver2018 a data.frame containing outputs from the Shriver 2018 
#' model for the historical period.
#' @param Future_Shriver2018 a data.frame containing outputs from the Shriver
#'  2018 model for the future period.
#' @param currYear numeric. The current year.
#' 
#' @return data.frame

formatShriver2018 <- function(Hist_Shriver2018, Future_Shriver2018, currYear) {

  Hist_Shriver2018$TP <- 'Historical'

  Future_Shriver2018$run_year <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 2)
  Future_Shriver2018$run_sim <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 1)
  Future_Shriver2018 <- Future_Shriver2018[Year %in% (currYear-1):currYear]
  Future_ShriverMeds <- Future_Shriver2018[, .(Prob = median(Prob)), .(run_sim, Year)]
  Future_ShriverMeds$TP <- as.character(Future_ShriverMeds$Year)

  return(bind(Hist_Shriver2018,Future_ShriverMeds))
}

#' Organizes all GISSM output for passing along API
#' 
#' @param Hist_Shriver2018 a data.frame containing outputs from the GISSM
#' model for the historical period.
#' @param Future_Shriver2018 a data.frame containing outputs from the GISSM
#'  model for the future period.
#' 
#' @return data.frame

formatGISSM <- function(Hist_GISSM, Future_GISSM) {

  Hist_GISSM$TP <- 'Historical'

  Future_GISSM$run_year <- sapply(strsplit(Future_GISSM$run, '_'), '[', 2)
  Future_GISSM$run_sim <- sapply(strsplit(Future_GISSM$run, '_'), '[', 1)
  Future_GISSM <- Future_GISSM[, .(Prob = mean(SeedlingSurvival_1stSeason)), .(run_sim, ryear)]
  Future_GISSM$TP <- as.character(Future_GISSM$ryear)

  return(bind(Hist_GISSM,Future_GISSM))
}

