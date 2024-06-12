#' Organizes variables necessary for Shriver 2018 model prediction
#'
#' @param TempDF a data.frame containing daily temperature data
#' @param VWCDF a data.frame containing daily VWC data
#'
#' @return data.frame

getShriver2018Vars <- function(TempDF, VWCDF) {
  
  # these condition are for the year AFTER seeding
  # For examples - if you planted in fall 2022  - we are looking at 2023 data for success
  
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
#' @param SoilsDF a data.frame containing soil texture information.
#' @param TimePeriod character. Future or Historical
#' @param currYear numeric. The current year.
#' @param currDate data. The current date.

#'
#' @return data.frame

getOConnor2020Vars <- function(sTempDF, VWCDF, SoilsDF, TimePeriod,
                               currYear = currYear,
                               currDate = currDate) {
  
  # Soil texture values
  sand <- as.numeric(SoilsDF[1, 'sand_frac'])
  clay <- as.numeric(SoilsDF[1, 'clay_frac'])
  
  if(TimePeriod == 'Future') {
    
    # subset so that data is either this year or next year
    sTempDF <- sTempDF[Year >= currYear, ]
    VWCDF <- VWCDF[Year >= currYear, ]
    
    # define periods of observed & forecast
    sTempDF$Date <- as.Date(strptime(paste(sTempDF$Year, sTempDF$Day), format="%Y %j"), format="%m-%d-%Y")
    VWCDF$Date <- as.Date(strptime(paste(VWCDF$Year, VWCDF$Day), format="%Y %j"), format="%m-%d-%Y")
    
    # elim more than 12 months out
    elimDate <- currDate + 366
    sTempDF <- sTempDF[sTempDF$Date < elimDate, ]
    VWCDF <- VWCDF[VWCDF$Date < elimDate, ]
    
    sTempDF$TP <- ifelse(sTempDF$Date >= currDate, 'Forecast', 'Observed')
    VWCDF$TP <- ifelse(VWCDF$Date >= currDate, 'Forecast', 'Observed')
    
  }
  
  if(TimePeriod == 'Historical') {
    sTempDF$TP <- 'Historical'
    VWCDF$TP <- 'Historical'
  }
  
  # soil temp: 0 - 5cm, mean and std. error * 1.96
  # SWP: 0 -5cm, mean and SE * 1.96
  # all days
  sTemp_top <- sTempDF[, c('Year', 'Day', 'Lyr_1_avg_C', 'TP')] # do we want the average temp? (is basically the same)
  sTemp_top <- makeDateMonthDat(sTemp_top, 'Day')
  sTemp_top <- sTemp_top[sTemp_top$Date %in% c(paste0('03-0',1:9),paste0('03-',10:31)) ,]
  
  # VWC mean
  # standard error of SWP needs to be calculated from SWP, not from VWC and converted to SWP
  VWC_top <- VWCDF[variable == 'Lyr_1', ]
  VWC_top <- makeDateMonthDat(VWC_top, 'Day')
  VWC_top <- VWC_top[VWC_top$Date %in% c(paste0('03-0',1:9),paste0('03-',10:31)) ,]
  
  VWC_top$SWP <- rSOILWAT2::swrc_vwc_to_swp(vwcBulk = VWC_top$value, sand = sand, clay = clay)
  VWC_top$SWP <- ifelse(VWC_top$SWP < -8, -8, VWC_top$SWP)
  
  # statistics taken in format step
  vars <- merge(sTemp_top[,c('Date', 'Year', 'TP','Lyr_1_avg_C')],
                VWC_top[,c('Date', 'Year', 'TP', 'SWP')],  by = c('Year','Date', 'TP'))
  
  return(vars)
  
}

