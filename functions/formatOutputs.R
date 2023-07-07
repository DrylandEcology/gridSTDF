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
  
  ## Data from HistData_MonthlyMeans_2 is associated with future months in this year.
  indx <- as.numeric(substr(todayMonthDay,1,2))
  HistData_MonthlyMeans_2$Year <- ifelse(as.numeric(substr(HistData_MonthlyMeans_2$Date, 1, 2)) >= indx,
                                         currYear, currYear + 1)
  HistData_MonthlyMeans_2 <- HistData_MonthlyMeans_2[Year == currYear, ]
  HistData_MonthlyMeans_2$Date <- as.Date(paste(HistData_MonthlyMeans_2$Year, 
                                                HistData_MonthlyMeans_2$Date, sep = '-'))
  
  #  Data from HistData_MonthlyMeans_2 is associated with future months in the NEXT year. (year 3) ----------------------------------------------
  HistData_MonthlyMeans_3$Year <- ifelse(as.numeric(substr(HistData_MonthlyMeans_3$Date, 1, 2)) < indx,
                                         currYear + 1, currYear)
  # Add one extra month for interpolation
  HistData_MonthlyMeans_3[HistData_MonthlyMeans_3$Year == currYear, 'Year'][1] <- currYear + 1
  
  HistData_MonthlyMeans_3 <- HistData_MonthlyMeans_3[Year == (currYear + 1), ]
  
  HistData_MonthlyMeans_3$Date <- as.Date(paste(HistData_MonthlyMeans_3$Year, 
                                                HistData_MonthlyMeans_3$Date,
                                                sep = '-'))
  
  # make one year record
  HistData_MonthlyMeans <- rbind(HistData_MonthlyMeans_2, HistData_MonthlyMeans_3)
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
formatOutputsMonthlys <- function(AllOut, SoilsDF, TP, yearBegin, yearEnd, currDate,
                                   todayMonthDay, currYearClimatology = FALSE) {
  
  # Subset Historical data
  if(TP == 'historical') AllOut <- AllOut[AllOut$Year %in% yearBegin:yearEnd, ]
  
  # Make Month dates -----------------------------------------------------------
  AllOut$Month <-  format(as.Date(strptime(paste(AllOut[,'Year'], AllOut[,'Day']), format="%Y %j")), format="%m")
  
  if(TP == 'future') {
    # first eliminate data that is before tday ... aka not the future
    AllOut$Date  <-  as.Date(strptime(paste(AllOut[,'Year'], AllOut[,'Day']), format="%Y %j"), format="%m-%d-%Y")
    AllOut <- subset(AllOut, Date >= currDate)
    AllOut$Date <- as.Date(paste(AllOut$Year, AllOut$Month, '15', sep = '-'))
  } else {
    # elminate data for currMonth ... if this sequence is a climatology for the current year
    if(currYearClimatology == TRUE){
      daysOut <- as.Date(todayMonthDay, format = "%m-%d")
      d <- day(daysOut) - 1
      daysOut <- seq(daysOut - d, (daysOut - 1), "days")
      daysOut <- format(daysOut, format="%m-%d")
      
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
                                .(run_year, Year, Date)]
    AllOutMean$ppt.mean <- NULL
    
    # for precip - sum per realization/rep first .. then get mean of that value
    AllOut$run <- paste(AllOut$run, AllOut$run_year, sep = '_')
    AllOutSum <- setDT(AllOut)[, .(ppt.sum = sum(ppt)), .(run, Year, Date)]
    AllOutSum$run_year <- as.numeric(sapply(strsplit(AllOutSum$run, '_'), '[', 2))
    AllOutSum <- setDT(AllOutSum)[, .(ppt.sum = mean(ppt.sum)), .(run_year,Year, Date)]
    
    AllOut <- merge(AllOutMean, AllOutSum)
    AllOut$run_year <- NULL
    
  } else {
    # take a mean of all vars (except ppt!)
    AllOutMean <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))),
                                .(Year, Date)]
    AllOutMean$ppt.mean <- NULL
    
    # for ppt ... get values per each year
    AllOutSum <- setDT(AllOut)[, .(ppt.sum = sum(ppt)), .(Year, Date)]
    
    AllOut <- merge(AllOutMean, AllOutSum)
    
  }
  
  AllOut$Year  <- NULL
  
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
  AllOut <- makeDateMonthDat(AllOut, 'Day')
  AllOut$Day <- AllOut$run <- NULL
  
  AllOut2 <- setDT(AllOut)[, sapply(.SD, function(x) list(mean=mean(x))), .(run_year, Year, Date)] # mean for every sim. year
  
  # Get rolling means and sums per simulation
  AllOut2 <- setorder(AllOut2, run_year, Year, Date)
  AllOut2 <- getRolling(AllOut2, TimePeriod = 'Future')
  
  # Make real dates
  AllOut2$Date <- as.Date(paste(AllOut2$Year, AllOut2$Date, sep = '-'))
  
  # Get quantiles of rolling
  AllOut2$run_year <- AllOut2$Year <- NULL
  
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
  # Step 1 - Join up recent past and historical climatologies
  # -----------------------------------------------------------------------------
  #### this includes calculating deltas for this past 6 months
  indx <- grep( paste0(Var, '.roll'), names(HistData1))
  Hist <- as.data.frame(HistData1)[,c(1, indx)]
  names(Hist)[2:4] <- paste0('Hist.', names(Hist)[2:4])
  
  # create date for joining
  Hist$Year <- ifelse(Hist$Date > todayMonthDay, currYear - 1, currYear )
  Hist$Date <- as.Date(paste(Hist$Year, Hist$Date, sep='-'), format = "%Y-%m-%d")
  Hist$Year <- NULL
  
  indx <- grep( paste0(Var, '.roll'), names(FutureData1))
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
  PastDailys[,paste0('RecentPast.',Var,'.diffs.med')] <- PastDailys[, indx[2]] - PastDailys[,indx[1]]
  
  # -----------------------------------------------------------------------------
  # Step 2 - Join Monthly Means  to calc diffs (no approx yet just monthlys) between future and historical climatologies
  # -----------------------------------------------------------------------------
  indx <- grep(paste0(Var), names(HistDataMonthly))
  Hist2 <- as.data.frame(HistDataMonthly)[, c(1, indx)]
  names(Hist2)[2:4] <- paste0('Hist.', names(Hist2)[2:4])
  
  indx <- grep( paste0(Var), names(FutureDataMonthly))
  Fut2 <- as.data.frame(FutureDataMonthly)[, c(1, indx)]
  names(Fut2)[2:4] <- paste0('NearFut.', names(Fut2)[2:4])
  
  FutMonths <- merge(Hist2, Fut2, by = 'Date')
  
  #  Calc deltas ---------------------------------------------------------------------
  
  # Differences between future and historical
  if(Var == 'ppt'){
    indxHist <- grep(paste0('Hist.',Var, '.sum.med'), names(FutMonths))
  } else {
    indxHist <- grep(paste0('Hist.',Var, '.mean.med'), names(FutMonths))
  }
  indxFuts <- grep('Fut', names(FutMonths))
  FutMonths[,paste0('NearFut.',Var,'.diffs.med')] <- FutMonths[, indxFuts[1]] - FutMonths[,indxHist]
  FutMonths[,paste0('NearFut.',Var,'.diffs.10')] <- FutMonths[, indxFuts[2]] - FutMonths[,indxHist]
  FutMonths[,paste0('NearFut.',Var,'.diffs.90')] <- FutMonths[, indxFuts[3]] - FutMonths[,indxHist]
  
  # -----------------------------------------------------------------------------
  # Step 3 - APPROX
  # -----------------------------------------------------------------------------
  
  # Create empty daily data frame for approximating across
  Fut_Daily_Approx <- data.frame(Date = seq(as.Date(currDate - 31), as.Date(currDate + 365), "days"))
  Fut_Daily_Approx <- plyr::join(Fut_Daily_Approx, FutMonths)
  
  yy <- dim(Fut_Daily_Approx)[2]
  
  for(i in 2:yy) {
    vector <- FutMonths[[i]]
    new <- approx(FutMonths$Date, vector, xout = Fut_Daily_Approx$Date,
                  rule = 2)$y
    
    Fut_Daily_Approx[[i]] <- new
  }
  
  Fut_Daily_Approx <- Fut_Daily_Approx[Fut_Daily_Approx$Date  > (currDate), ]
  
  # --------------------------------------------------------------------------
  # Step 4 - Format and Combine per variable
  # --------------------------------------------------------------------------
  # From PastDailys DF -------------------------------------------------------
  ## Hist.Var_rollmean.med - elim
  ## Hist.Var_rollmean.10 - elim
  ## Hist.Var_rollmean.90 - elim
  ## RecentPast.Var.mean.med = past dailys
  ## RecentPast.Var_rollmean.med = thick dailys
  ## RecentPast.Var_C.Diffs.Med - Panel two, left of purple line
  
  indx <- grep('Hist.', names(PastDailys))
  PastDailys[,indx] <- NULL
  PastDailys[PastDailys$Date > (lastWeatherDate - 15), 3:4] <- NA
  
  #  From Fut_Daily_Approx -------------------------------------------------
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
  indx <- grep(paste0(Var, '.roll'), names(HistDataNormMean_18MNs))
  Hist18MNs <- HistDataNormMean_18MNs[, c(1,indx)]
  indx1 <- grep('.med', names(Hist18MNs), fixed = TRUE)
  indx2 <- grep('.10', names(Hist18MNs))
  indx3 <- grep('.90', names(Hist18MNs))
  Hist18MNs[,paste0(Var,'.roll.10.diff')] <- Hist18MNs[, indx2] - Hist18MNs[,indx1]
  Hist18MNs[,paste0(Var,'.roll.90.diff')] <- Hist18MNs[, indx3] - Hist18MNs[,indx1]
  
  # Join
  AllDailys <- data.frame(Date = seq(as.Date(currDate - 183),as.Date(currDate + 365), "days"))
  AllDailys <- plyr::join(AllDailys, Hist18MNs)
  AllDailys <- plyr::join(AllDailys, PastDailys)
  AllDailys <- plyr::join(AllDailys, Fut_Daily_Approx)
  
  return(AllDailys)
  
}


