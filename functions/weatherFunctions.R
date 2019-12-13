getWeatherData <- function(lat, lng) {
  
  stencil <- simplegeom(c(lng, lat))
  fabric = webdata(url='https://cida.usgs.gov/thredds/dodsC/UofIMETDATA')
  variables(fabric) <- c('precipitation_amount', 'min_air_temperature', 'max_air_temperature')
  times(fabric) <- c('1979-01-01','2018-12-31')
  job <- geoknife(stencil, fabric, wait = TRUE)
  
  # format
  data <- result(job)
  data$statistic <- NULL
  wdata <- reshape2::dcast(data, DateTime ~ variable, value.var = 'bufferedPoint')
  #   Year DOY Tmax_C Tmin_C PPT_cm
  wdata$Year <- year(wdata$DateTime)
  wdata$DOY <- yday(wdata$DateTime)
  wdata$max_air_temperature <- wdata$max_air_temperature - 273.15
  wdata$min_air_temperature <- wdata$min_air_temperature - 273.15
  wdata$precipitation_amount <- wdata$precipitation_amount/10
  
  wdata$DateTime <- NULL
  wdata <- wdata[,c('Year', 'DOY', 'max_air_temperature', 'min_air_temperature', 'precipitation_amount' )]
  names(wdata) <- c('Year', 'DOY','Tmax_C', 'Tmin_C', 'PPT_cm')
  
  return(wdata)
}

getWeatherCoefficientsFromHistorical <- function(wdata) {
  
  # Step 1 - standardized coefficients from historical data
  wdata2 <- wdata[wdata$Year %in% c(1980:2010),]
  
  # Generate coefss
  res2 <- dbW_estimate_WGen_coefs(wdata2)
  res2[["mkv_doy"]]$PPT_sd[is.na(res2[["mkv_doy"]]$PPT_sd)] <- 0 #this is an error - email Daniel
  
  return(res2)
}

getFormatAnomalies <- function(lat, lng, wdata){
  
  # Format historical data ----------------------------------------------------------------------
  wdata2 <- wdata[wdata$Year %in% c(1980:2010),]
  
  # Create data.frames necessary for data transformation down the road 
  wdata2$MN <- month(strptime(paste(wdata2$Year, wdata2$DOY), format = "%Y %j"))
  ## Temp - min and maximum temperature
  wdataMonthTemp <- setDT(wdata2)[, .(MaxTemp = mean(Tmax_C), MinTemp = mean(Tmin_C)), .(MN)]
  
  # PPT - need to eliminate dats without PPT
  wdata2[wdata2$PPT_cm ==0, 'PPT_cm'] <- NA
  wdataDOY <-  setDT(wdata2)[, .( PPT = mean(PPT_cm, na.rm = TRUE)), .(DOY)]
  wdataDOY$PPT[366] <- 0
  wdataDOY$MN <- month(as.Date(wdataDOY$DOY, origin = "2016-01-01"))
  wdataMonthPPT <- setDT(wdataDOY)[, .(MonthlyPPT =sum(PPT,na.rm=TRUE)), .(MN)]
  
  #Determine Region from coordinates and shapefile ------------------------------------------
  CD102 <- shapefile(x = 'CD102/CD102.shp')
  points <- data.frame(x = lng, y = lat)
  coordinates(points) <- ~ x + y 
  proj4string(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  CDRegion <- as.numeric(over(points, CD102)$ID4)
  
  # Read in and subset anomaly data -----------------------------------------------------------
  TempMonthlyAnomsWhole <- fread('CurrentAnomalyTempData.csv')
  TempMonthlyAnoms <- subset(TempMonthlyAnomsWhole, CD == CDRegion)

  TempMonthlyAnoms <- plyr::join(wdataMonthTemp, TempMonthlyAnoms)
  TempMonthlyAnoms$Max_Temp_Forecast <- TempMonthlyAnoms$MaxTemp + TempMonthlyAnoms$MeanAnomaly
  TempMonthlyAnoms$Min_Temp_Forecast <- TempMonthlyAnoms$MinTemp + TempMonthlyAnoms$MeanAnomaly
  
  PPTMonthlyAnomsWhole <- fread('CurrentAnomalyPPTData.csv')
  PPTMonthlyAnoms <- subset(PPTMonthlyAnomsWhole, CD == CDRegion)
  
  PPTMonthlyAnoms <- plyr::join(wdataMonthPPT, PPTMonthlyAnoms)
  PPTMonthlyAnoms$PPT_Forecast <- PPTMonthlyAnoms$MonthlyPPT + PPTMonthlyAnoms$MeanAnomaly
  
  return(list(TempMonthlyAnoms, PPTMonthlyAnoms))
  
}

integrateAnomalyData <- function(res2, TempMonthlyAnoms, PPTMonthlyAnoms){
  
  # This function alters the coefficients in the weather generator
  ## Temp -> Monthly anomaly to weekly coefs
  ## PPT -> Monthly anomaly to daily coefs
  
  ### Temp ------------------------------------------------------------------------------------ 
  ####### Fit a function to monthly data and interpolate with splines to weekly.
  mon <- 1:12
  week <- 1:53
  
  # Fit interplation spline to monthly data  and predict weekly values (but this is just mean, not min and max)
  maxWeeks <- stats::spline(mon, TempMonthlyAnoms$Max_Temp_Forecast, 
                          xout = week / max(week) * max(mon))
  minWeeks <- stats::spline(mon, TempMonthlyAnoms$Min_Temp_Forecast, 
                            xout = week / max(week) * max(mon))
  
  # Enter into weather anomaly data
  res2[['mkv_woy']]$wTmax_C <- maxWeeks$y
  res2[['mkv_woy']]$wTmin_C <- minWeeks$y

  
  ### Precip ----------------------------------------------------------------------------
  # Multiplicative scale totals ------------------------------------------------
  # # Coef data
  PPTData <- data.frame(DOY = res2$mkv_doy$DOY, PPT = res2$mkv_doy$PPT_avg, SD = res2$mkv_doy$PPT_sd)
  PPTData$MN <- month(as.Date(PPTData$DOY, origin = "2016-01-01"))

  # Join and scale every day to new monthly values
  PPTData2 <- plyr::join(PPTData, PPTMonthlyAnoms)
  ppt_scaled <-  PPTData2$PPT * (PPTData2$PPT_Forecast/PPTData2$MonthlyPPT)
  
  #PPTData2$ppt_scaled <-  PPTData2$PPT * (PPTData2$PPT_Forecast/PPTData2$MonthlyPPT)
  #Check <- setDT(PPTData2)[,.(NewMonthly = sum(ppt_scaled)), .(MN, PPT_Forecast)]
 
  # Wet day standard deviation is trickier. A first approximation could be to not change it which may be reasonable approach.
  # However, we also know that SD tends to get larger with more precipitation. Thus, a second approach could be to fit a relationship SD ~ ppt (of wet days) on the existing data,
  # then scale ppt as described above, and use the fitted function to predict how much larger the SD of the scaled ppt value should be.
  # 1 Relationship between SD of ppt and average ppt - f(ppt) = SD ~ s(ppt)
  
  is_wetday <- res2[["mkv_doy"]][, "PPT_avg"] > 0
  ppt <- res2[["mkv_doy"]][is_wetday, "PPT_avg"]
  SD <- res2[["mkv_doy"]][is_wetday, "PPT_sd"]
  
  # Fit spline regression
  m <- lm(SD ~ bs(ppt)) # cubic splines

  # Calculate scaling for SD
  SD_scaled <- SD * predict(m, data.frame(ppt = ppt_scaled)) / predict(m, data.frame(ppt = ppt))
  
  res2[["mkv_doy"]]$PPT_avg <- ppt_scaled
  res2[["mkv_doy"]]$PPT_sd <- SD_scaled

  return(res2)  
}


