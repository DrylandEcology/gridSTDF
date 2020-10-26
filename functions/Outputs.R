#' get outputs from SOILWAT objects
#'
#'  @param sw_out SOILWAT2 output object
#'  @param sw_in SOILWAT2 input object
#'  @param SoilsDF a data.frame describing soils attributes, including depth grouping
#'  @param calc_EcoVars logical. Do you want to calculate  ecological variables
#'
#'  @return A list of data.frames.
getOutputs <- function(sw_out, sw_in, SoilsDF, calc_EcoVars = TRUE) {

  # Temp and Precip
  Temp1 <- data.table(sw_out@TEMP@Day)
  Temp1 <- Temp1[, c('Year', 'Day', 'avg_C')]

  PPT1 <-  data.table(sw_out@PRECIP@Day)
  PPT1 <- PPT1[, c('Year', 'Day', 'ppt')]

  # VWC ---------------------------------------------------
  VWC1 <-  data.table(sw_out@VWCMATRIC@Day)
  VWC1 <- melt.data.table(VWC1, id.vars = c('Year', 'Day'))
  
  # Soil Temperature
  sTemp <- data.table(sw_out@SOILTEMP@Day)
  
  # Get EcoVars ----------------------------------------------------------------

  if(calc_EcoVars) {
    
    # Shriver 2018  Vars
    Shriver2018Vars <- getShriver2018Vars(Temp1, VWC1)

    #OConnor Vars
    Oconnor2020Vars <- getOConnor2020Vars(sTemp, VWC1, SoilsDF)
    
    # GISSM Vars
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
  # Step 1 - Get weighted mean across depths for VWC
  
  VWC1 <-  merge(VWC1, SoilsDF)#, by = 'variable')
  VWC1 <- setDT(VWC1)[,.(VWC = weighted.mean(value, width)),
                      .(Year, Day, Depth)]
  VWC1$Depth <- paste0('VWC.', VWC1$Depth)
  VWC1 <- dcast(VWC1, Year +  Day ~ Depth, value.var = 'VWC')

  # Join up
  Data <- merge(Temp1, PPT1, by = c('Year', 'Day'))
  Data <- merge(Data, VWC1,  by = c('Year', 'Day'))

  if(!calc_EcoVars) return(list(Data))
  if(calc_EcoVars) return(list(Data, Shriver2018Vars, 
                               data.table(GISSM_1[[1]]), Oconnor2020Vars))

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
#' @param currDate. Date. today's date/
#' @param todayMonthDay Date. in month-day format.
#' @param currYearClimatology logical. Is this the climatology representing the current year
#'
#' @return a data.frame containing monthly quantiles (10th, 50th, 90th) for
#' climate and soil moisture variables
#'
formatOutputs_Monthlys <- function(AllOut, SoilsDF, TP, yearBegin, yearEnd, currDate,
                                   todayMonthDay, currYearClimatology = FALSE) {

  # Subset Historical data
  if(TP == 'historical') AllOut <- AllOut[AllOut$Year %in% yearBegin:yearEnd, ]

  # Make Month dates -----------------------------------------------------------
  if(TP == 'future')  AllOut$run_year <- sapply(strsplit(AllOut$run, '_'), '[', 2)
  AllOut$Month <-  format(as.Date(strptime(paste(AllOut$Year, AllOut$Day), format="%Y %j")), format="%m")

  if(TP == 'future') {
    # first eliminate data that is before tday ... aka not the future
    AllOut$Date <-  as.Date(strptime(paste(AllOut$Year, AllOut$Day), format="%Y %j"), format="%m-%d-%Y")
    AllOut <- subset(AllOut, Date >= currDate)
    AllOut$Date <- as.Date(paste(AllOut$Year, AllOut$Month, '15', sep = '-'))
  } else{
    # elminate data for currMonth ... if this sequence is a climatology for the current year
    if(currYearClimatology == TRUE){
      daysOut <- as.Date(todayMonthDay, format = "%m-%d")
      d <- day(daysOut) - 1
      daysOut <- as.Date((daysOut - d):(daysOut - 1))
      daysOut <- format(daysOut, format = "%m-%d")

      AllOut <- makeDateMonthDat(AllOut, 'Day')
      AllOut <- AllOut[!AllOut$Date %in% daysOut,]
      AllOut$Date <- NULL
    }
    Date <- paste(AllOut$Month, '15', sep = '-')
    AllOut <- cbind(Date, AllOut)
  }

  AllOut$Month <-  AllOut$Day <- NULL

  # mean for every sim. year month ---------------------------------------------
  if(TP == 'future') {
    # take a mean across all realizations and representations for most all vars
    AllOutMean <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))),
                                .(run_year, Year, Date),
                                .SDcols = c('avg_C', 'VWC.Deep', 'VWC.Intermediate',
                                            'VWC.Shallow')]
    # for precip - sum per realization/rep first .. then get mean of that value
    AllOutSum <- setDT(AllOut)[, .(ppt.sum = sum(ppt)), .(run, Year, Date)]
    AllOutSum$run_year <- sapply(strsplit(AllOutSum$run, '_'), '[', 2)
    AllOutSum <- setDT(AllOutSum)[, .(ppt.sum = mean(ppt.sum)), .(run_year,Year, Date)]

    AllOut <- merge(AllOutMean, AllOutSum)
    AllOut$run_year <-  NULL

  } else{
    # take a mean across all realizations and representations for most all vars
    AllOutMean <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))),
                                .(Year, Date),
                                .SDcols = c('avg_C', 'VWC.Deep', 'VWC.Intermediate',
                                            'VWC.Shallow')]
    # for ppt ... get values per each year
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

  return(AnomRunStats)

}

#' Get an 18 month time series of the climatolgical records.
#'
#' Takes into account that different climatologies are used for predictions in
#' in different years. For example, predictions for the current year use 1980 - 2010
#' to assist in predicting future conditions, while the next year uses 1981 - 2011.
#'
#' @param HistData_Norm_Stats1 data.frame that will account for six months in the past
#' @param HistData_Norm_Stats2 data.frame that will account for dates in the current year
#' @param HistData_Norm_Stats3 data.frame that will account dates in the next year
#' @param currDate Current date.
#' @param currMonth Current month
#' @param currYear Current year
#' @param todayMonthDay today's date in the format MM-DD
#'
#' @return data.frame containing 18 month time series, 6 months in past, 12 in the
#' future.

get18MonthClimatologicalRecord <- function(HistData_Norm_Stats1,
                                           HistData_Norm_Stats2,
                                           HistData_Norm_Stats3,
                                           currDate, currMonth, currYear,
                                           todayMonthDay) {
  ##### ------------------------------------------------------------------------------
  # Hist Norms --------------------------------------------------------------------------
  ##### ------------------------------------------------------------------------------

  #  Get data for prior 6 months
  sixMonthsAgo <- format(currDate - 183,  format="%m-%d")
  r1 <- which(grepl(todayMonthDay, HistData_Norm_Stats1$Date))
  r2 <- which(grepl(sixMonthsAgo, HistData_Norm_Stats1$Date))
  dateIndex <- if( r2 > r1) {
    c(r2:366, 1:(r1- 1))
  } else{
    c(r2:(r1 - 1))
  }

  #  Format historical record for "Last 6 Months" ----------------------------------------------
  ### For before today (all years)
  HistData_Norm_Stats1 <- HistData_Norm_Stats1[dateIndex,]
  HistData_Norm_Stats1$Year <- ifelse(HistData_Norm_Stats1$Date > todayMonthDay, currYear - 1 , currYear)
  HistData_Norm_Stats1$Date <- as.Date(paste(HistData_Norm_Stats1$Year, HistData_Norm_Stats1$Date, sep = '-'))
  HistData_Norm_Stats1 <- setorder(HistData_Norm_Stats1, Date)

  #  Format historical record for "Curr Year' (year 2) ----------------------------------------------
  ## Current Year - Future
  HistData_Norm_Stats2$Year <- ifelse(HistData_Norm_Stats2$Date >= todayMonthDay, currYear, currYear + 1)
  HistData_Norm_Stats2 <- HistData_Norm_Stats2[Year == currYear, ]
  HistData_Norm_Stats2$Date <- as.Date(paste(HistData_Norm_Stats2$Year, HistData_Norm_Stats2$Date, sep = '-'))

  #  Format historical record for "Next Year' (year 3) ----------------------------------------------
  HistData_Norm_Stats3$Year <- ifelse(HistData_Norm_Stats3$Date > todayMonthDay, currYear, currYear + 1)
  HistData_Norm_Stats3 <- HistData_Norm_Stats3[Year == (currYear + 1), ]
  HistData_Norm_Stats3$Date <- as.Date(paste(HistData_Norm_Stats3$Year, HistData_Norm_Stats3$Date, sep = '-'))

  # Append
  HistDataNormMean_18MNs <- rbind(HistData_Norm_Stats1, HistData_Norm_Stats2, HistData_Norm_Stats3)

  # Go to the first day of the same month of the next year
  HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < as.Date(paste0(currYear + 1, '-' ,currMonth + 1,'-01')),]

  return(HistDataNormMean_18MNs)
}

#' Format historical monthlys for deltas calculations taking into account
#' varying climatologies.
#'
#' @param HistData_MonthlyMeans_2 data,frame containing historical monthly values
#' @param HistData_MonthlyMeans_3 data,frame containing historical monthly values
#' @param currYear numeric. Current year
#' @param todayMonthDay today's date in the format MM-DD
#'
#' @return data.frame containing a year's worth of monthly delta data.

formatHistoricalMonthlys <- function(HistData_MonthlyMeans_2,
                                                HistData_MonthlyMeans_3,
                                                currYear, todayMonthDay) {

  ##### ------------------------------------------------------------------------------
  # Monthly Means ---------------------------------------------------------------------
  ##### ------------------------------------------------------------------------------

  ## Current Year - Future
  indx <- as.numeric(substr(todayMonthDay,1,2))
  HistData_MonthlyMeans_2$Year <- ifelse(as.numeric(HistData_MonthlyMeans_2$Date) >= indx, currYear, currYear + 1)
  HistData_MonthlyMeans_2 <- HistData_MonthlyMeans_2[Year == currYear, ]
  HistData_MonthlyMeans_2$Date <- as.Date(paste(HistData_MonthlyMeans_2$Year, HistData_MonthlyMeans_2$Date, sep = '-'))

  #  Format historical record for "Next Year' (year 3) ----------------------------------------------
  HistData_MonthlyMeans_3$Year <- ifelse(HistData_MonthlyMeans_3$Date > todayMonthDay, currYear, currYear + 1)
  # Add one extra month for interpolation
  HistData_MonthlyMeans_3[HistData_MonthlyMeans_3$Year == currYear, 'Year'][1] <- currYear + 1

  HistData_MonthlyMeans_3 <- HistData_MonthlyMeans_3[Year == (currYear + 1), ]
  HistData_MonthlyMeans_3$Date <- as.Date(paste(HistData_MonthlyMeans_3$Year, HistData_MonthlyMeans_3$Date, sep = '-'))

  # make one year record
  HistData_MonthlyMeans <- rbind(HistData_MonthlyMeans_2, HistData_MonthlyMeans_3)
}


#' Format and return outputs from future SOILWAT2 runs
#'
#' @param AllOut data.frame containing results from all future simulations.
#' @param SoilsDF data.frame contains soils information include texture and depth.
#' @param currDate. current date.
#'
#' @return data.frame containing formating (quantiles of rolling means) of future
#' simulation data
#'
formatOutputsFuture <- function(AllOut, SoilsDF, currDate) {

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

  sixMonthsAgo <- format(currDate - 183)
  AnomRunStats <- AnomRunStats[AnomRunStats$Date >= sixMonthsAgo, ] # 6 month lead ins
  AnomRunStats <- AnomRunStats[AnomRunStats$Date <  currDate + 396, ]

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
  Data2$avg_C_rollmean <- runmean(Data2$avg_C,  k = 30, endrule = 'mean', align = 'center')#
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
#' @param VWCDF a data.frame containing daily VWC data
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

#' Organizes variables necessary to create figures based on Oconnors 2019 ERL
#' paper. This includes to mean and standard error of soil temp and SWP in the
#' 0-5cm soil layer.
#'
#' @param TempDF a data.frame containing daily soil temperature data
#' @param SWPDF a data.frame containing daily swp data
#'
#' @return data.frame

getOConnor2020Vars <- function(sTempDF, VWCDF, SoilsDF) {
  
  std <- function(x) sd(x)/sqrt(length(x))
  
  # Soil texture values
  sand <- as.numeric(SoilsDF[1, 'sand_frac'])/100
  clay <- as.numeric(SoilsDF[1, 'clay_frac'])/100
  
  # soil temp: 0 - 5cm, mean and std. error * 1.96
  # SWP: 0 -5cm, mean and SE * 1.96
  # all days

  sTemp_top <- sTempDF[,.(sTemp_mean = mean(Lyr_1),
                          sTemp_CI95 = std(Lyr_1) * 1.96),
                          .(Day)]
  
  # VWC mean
  # standard error of SWP needs to be calculated from SWP, not from VWC and converted to SWP
  VWC_top <- VWCDF[variable == 'Lyr_1', ]
  
  VWC_top$SWP <- rSOILWAT2::VWCtoSWP(VWC_top$value, sand, clay)
  VWC_top$SWP <- ifelse(VWC_top$SWP < -8, -8, VWC_top$SWP)
  VWC_top <- VWC_top[,.(SWP_mean = mean(SWP),
                       SWP_CI95 = std(SWP) * 1.96),
                     .(Day)]
  
  # will be converted to SWP in final formatting
  
  vars <- merge(sTemp_top, VWC_top,  by = c('Day'))

  return(vars)
  
}


#' Calculates monthly deltas between datasets, approximates daily values, and
#' formats data
#'
#' @param HistData1 A data.frame containing historical climatologies.
#' @param HistDataMonthly A data.frame containing historical monthly means.
#' @param HistDataNormMean_18MNs A data.frame with the 18month long time series
#' of historical data
#' @param FutureData1 A data.frame containing future climatologies.
#' @param FutureDataMonthly A data.frame containing future monthly means.
#' @param Var character. Name of variable which deltas are being calculated for
#' @param currDate date. Today's date.
#' @param todayMonthDay
#' @param currYear numeric. Current year.
#' @param lastWeatherDate Date. Date gridMet data was last uploaded
#'
#' @return data.frame with properly formatted outputs.

calcDeltasApproxAndFormat <- function(HistData1, HistDataMonthly,
                                      HistDataNormMean_18MNs,
                                      FutureData1, FutureDataMonthly,
                                      Var, currDate, todayMonthDay, currYear,
                                      lastWeatherDate) {

  # -----------------------------------------------------------------------------
  # Step 1 - Join Dailys from before today's date (RecentPast) to reflect actual patterns (no approx / monthlys)
  # -----------------------------------------------------------------------------
  #### this includes calculating deltas for this past 6 months

  indx <- grep( paste0(Var, '_roll'), names(HistData1))
  Hist <- as.data.frame(HistData1)[,c(1, indx)]
  names(Hist)[2:4] <- paste0('Hist.', names(Hist)[2:4])

  # ccreate date for joinging
  Hist$Year <- ifelse(Hist$Date > todayMonthDay, currYear, currYear - 1 )
  Hist$Date <- as.Date(paste(currYear, Hist$Date,sep='-'), format = "%Y-%m-%d")
  Hist$Year <- NULL

  indx <- grep( paste0(Var, '_roll'), names(FutureData1))
  indx2 <- grep( paste0(Var, '.mean.med'), names(FutureData1))
  Fut <-  as.data.frame(FutureData1)[,c(1, indx2, indx)]
  indx <- grep('.10|.90', names(Fut), invert = TRUE)
  Fut <- Fut[,indx]
  names(Fut)[2:3] <- paste0('RecentPast.', names(Fut)[2:3])

  PastDailys <- merge(Hist, Fut)

  #  Calc deltas ---------------------------------------------------------------------
  # Differences between future and historical - thick yellow line
  if(Var == 'ppt'){
    indx <- grep('rollsum.med', names(PastDailys))
  } else {
    indx <- grep('rollmean.med', names(PastDailys))
  }
  PastDailys[,paste0('RecentPast.',Var,'.Diffs.Med')] <- PastDailys[, indx[2]] - PastDailys[,indx[1]]

  # -----------------------------------------------------------------------------
  # Step 2 - Join Monthly Means and to calc diffs (no approx yet just monthlys) for future
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
  if(Var == 'ppt'){
    indxHist <- grep(paste0('Hist.',Var, '.sum.med'), names(FutMonths))
  } else {
    indxHist <- grep(paste0('Hist.',Var, '.mean.med'), names(FutMonths))
  }
  indxFuts <- grep('Fut', names(FutMonths))
  FutMonths[,paste0('NearFut.',Var,'.Diffs.Med')] <- FutMonths[, indxFuts[1]] - FutMonths[,indxHist]
  FutMonths[,paste0('NearFut.',Var,'.Diffs.10')] <- FutMonths[, indxFuts[2]] - FutMonths[,indxHist]
  FutMonths[,paste0('NearFut.',Var,'.Diffs.90')] <- FutMonths[, indxFuts[3]] - FutMonths[,indxHist]

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
                  rule = 2)$y

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
  PastDailys[PastDailys$Date > (lastWeatherDate-15), 3:4] <- NA

  # From Fut_Daily_Approx -------------------------------------------------
  ## Hist.avg_C.mean.med - elim
  ## Hist.avg_C.mean.10 - elim
  ## Hist.avg_C.mean.90 - elim
  ## "Fut.avg_C.mean.med" - Panel 1, blue
  ## "Fut.avg_C.mean.10" - Panel 1
  ## "Fut.avg_C.mean.90" - Panel 1

  ## "NearFut.avg_C.Diffs.Med" - Panel 2
  ## "NearFut.avg_C.Diffs.10" - Panel 2
  ## "NearFut.avg_C.Diffs.90" - Panel 2

  indx <- grep('Hist.', names(Fut_Daily_Approx))
  Fut_Daily_Approx[,indx] <- NULL
  Fut_Daily_Approx[Fut_Daily_Approx$Date < (currDate + 15), 2:7] <- NA

  # Add data from 18 months time series so all data to make figures is one data.frame
  # avg_C_rollmean.med - black line
  # avg_C_rollmean.10 - gray boundary
  # avg_C_rollmean.90 - gray boundary
  # Hist.avg_C_roll.10.diff - gray boundary, bottom panel
  # Hist.avg_C_roll.90.diff - gray boundary, bottom panel
  indx <- grep(paste0(Var, '_roll'), names(HistDataNormMean_18MNs))
  Hist18MNs <- HistDataNormMean_18MNs[, c(1,indx)]
  indx1 <- grep('.med', names(Hist18MNs), fixed = TRUE)
  indx2 <- grep('.10', names(Hist18MNs))
  indx3 <- grep('.90', names(Hist18MNs))
  Hist18MNs[,paste0(Var,'_roll.10.diff')] <- Hist18MNs[, indx2] - Hist18MNs[,indx1]
  Hist18MNs[,paste0(Var,'_roll.90.diff')] <- Hist18MNs[, indx3] - Hist18MNs[,indx1]

  # Join
  AllDailys <- data.frame(Date = as.Date(as.Date(currDate - 183):as.Date(currDate + 365)))
  AllDailys <- plyr::join(AllDailys, Hist18MNs)
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
#' @param Hist_GISSM a data.frame containing outputs from the GISSM
#' model for the historical period.
#' @param Future_GISSM a data.frame containing outputs from the GISSM
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

#' Organizes all OConnor2020 output for passing along API
#'
#' @param Hist_OConnor2020 a data.frame containing outputs for the OConnor2020
#' model for the historical period.
#' @param Future_OConnor2020 a data.frame containing outputs for the OConnor2020
#'  model for the future period.
#'
#' @return data.frame

formatOConnor2020 <- function(Hist_OConnor2020, Future_OConnor2020) {

  # Historical -----------------------------------------------------------------
  Hist_OConnor2020$TP <- 'Historical'
  # Future --------------------------------------------------------------------
  Future_OConnor2020  <- Future_OConnor2020[, lapply(.SD, mean), .(Day)]
  Future_OConnor2020$TP <- 'Future'
  # All
  All <- rbind(Hist_OConnor2020,Future_OConnor2020)
  All <- All[All$Day != 366,]
  
  return(All)
}
