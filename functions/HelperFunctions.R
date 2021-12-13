# year 1 is last years data
# thisYearObservedWData is this year's observed weather data
makeWeathOneSim <- function(y, year1, thisYearObservedWData, weathAnomAll,
                             currDOY, currYear) {

  days <- dim(thisYearObservedWData)[1]
  daysNextYear <- yearDays(as.Date(paste0(currYear + 1,'-01-01')))
  LeapYear <- days == 366

  ### ---------
  ### year 2 - observed data for this year until today's date and then future, weathAnom data
  ### ---------

  year2  <- data.frame(Year = currYear, DOY = 1:days)
  year2 <- makeDateMonthDat(year2, 'DOY')

  # join future data to the structure of this year's data
  year2Fut <- weathAnomAll[weathAnomAll$Year == y , ]
  year2Fut <- makeDateMonthDat(year2Fut, 'DOY')
  year2 <- plyr::join(year2, year2Fut[,c('Date', 'Tmax_C', 'Tmin_C', 'PPT_cm')],
                      by = 'Date')

  # fill in with this years' observed data
  # should have the correct number of days always since they are good representation of the same year
  year2[1:currDOY, c('Tmax_C', 'Tmin_C', 'PPT_cm')] <- thisYearObservedWData[1:currDOY, c('Tmax_C', 'Tmin_C', 'PPT_cm')]

  if(any(is.na(year2))) {
    print('NAs present in year2 data')
    year2$Tmax_C <- zoo::na.fill( year2$Tmax_C, "extend")
    year2$Tmin_C <- zoo::na.fill( year2$Tmin_C, "extend")
    year2$PPT_cm <- zoo::na.fill( year2$PPT_cm, "extend")
  }
   # ggplot() +
   #   geom_line(data = year2, aes(Date, Tmax_C, group = 1)) +
   #   geom_line(data = thisYearObservedWData, aes(Date, Tmax_C, group = 1), color = 'purple') +
   #   geom_line(data = year2Fut, aes(Date, Tmax_C, group = 1), color = 'green')


  ### ---------
  ## year 3 ... forecasts that run into next year (aka 2021) and then scratch data for the rest of 2021
  ### ---------
  year3  <- data.frame(Year = currYear + 1, DOY = 1:daysNextYear)
  year3 <- makeDateMonthDat(year3, 'DOY')

  # join future data to the structure of this year's data
  year3Fut <-  weathAnomAll[weathAnomAll$Year == y + 1, ]
  year3Fut <- makeDateMonthDat(year3Fut, 'DOY')
  year3 <- plyr::join(year3, year3Fut[,c('Date', 'Tmax_C', 'Tmin_C', 'PPT_cm')],
                      by = 'Date')

  # year3OG <- wdata[wdata$Year == y + 1, ]
  # year3OG <- makeDateMonthDat(year3OG, 'DOY')
  #
  # ggplot() +
  #   geom_line(data = year3OG, aes(Date, Tmax_C, group = 1), color = 'green') +
  #   geom_line(data = year3, aes(Date, Tmax_C, group = 1), lty = 'dashed')
  #
  # year3$Tmax_C - year3OG$Tmax_C
  #

  # Make one simulation's worth of data
  year2$Date <- year3$Date <- NULL
  weathAnomOneSim <- rbind(year1, year2, year3)

  return(weathAnomOneSim)
}

makeDateMonthDat <- function(dataSet, dayName){
  dataSet <- as.data.frame(dataSet)
  dataSet$Date <- as.Date(strptime(paste(dataSet$Year, dataSet[,dayName]), format="%Y %j"), format="%m-%d-%Y")
  dataSet$Date <- format(dataSet$Date, format="%m-%d")
  return(dataSet)
}

yearDays <- function(time) {
  {
    time <- as.POSIXlt(time)
    time$mon[] <- time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
    time$year <- time$year + 1
    return(as.POSIXlt(as.POSIXct(time))$yday + 1)
  }
}

makeMonthLeadRelationshipTable <- function(TempAnoms, currMonth) {

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

std <- function(x) sd(x)/sqrt(length(x))
