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
  ll <- length(names(HistData_Norm_Stats))
  HistData_Norm_Stats <- setnames(setDT(HistData_Norm_Stats)[ ,sapply(.SD, function(x) list(med=median(x, na.rm = TRUE),
                                                                                            x10=quantile(x, .1, na.rm = TRUE),
                                                                                            x90 = quantile(x, .9, na.rm = TRUE))),
                                                              .(Date)],
                                  c('Date', sapply(names(HistData_Norm_Stats)[-c(ll)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!
  HistData_Norm_Stats <- getSWP(HistData_Norm_Stats, SoilsDF)
  HistData_Norm_Stats <- setorder(HistData_Norm_Stats, Date)
  
  return(HistData_Norm_Stats)
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
  ##### ------------------------------------------------------------------------
  # Hist Norms -----------------------------------------------------------------
  ##### ------------------------------------------------------------------------
  
  #  Get data for prior 6 months
  sixMonthsAgo <- format(currDate - 183,  format="%m-%d")
  r1 <- which(grepl(todayMonthDay, HistData_Norm_Stats1$Date))
  r2 <- which(grepl(sixMonthsAgo, HistData_Norm_Stats1$Date))
  
  d_index <- if( r2 > r1) {
    c(r2:366, 1:(r1- 1))
  } else{
    c(r2:(r1 - 1))
  }
  
  #  Format historical record for "Last 6 Months" ----------------------------------------------
  ### For before today (all years)
  HistData_Norm_Stats1 <- HistData_Norm_Stats1[d_index,]
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
  d_indx <- if(currMonth != 12) {
    paste0(currYear + 1, '-' , currMonth + 1,'-01')
  } else {
    paste0(currYear + 2, '-01', '-01')
  }
  HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < as.Date(d_indx),]
  
  return(HistDataNormMean_18MNs)
}