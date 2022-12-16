#' get outputs from SOILWAT objects
#'
#'  @param sw_out SOILWAT2 output object
#'  @param sw_in SOILWAT2 input object
#'  @param SoilsDF a data.frame describing soils attributes, including depth grouping
#'  @param calc_EcoVars logical. Do you want to calculate ecological variables?
#'  @param TimePeriod character. Future or Historical
#'  @param currYear numeric. The current year.
#'  @param currDate data. The current date.
#'
#'  @return A list of data.frames.
getOutputs <- function(sw_out, sw_in, SoilsDF, calc_EcoVars = TRUE,
                       TimePeriod, currYear, currDate, run_year, nn) {

  # Temp and Precip
  Data <- data.table(sw_out@TEMP@Day[, c('Year', 'Day', 'avg_C')], 
                'ppt' = sw_out@PRECIP@Day[,'ppt'])

  # VWC ---------------------------------------------------
  VWC1 <-  data.table(sw_out@VWCMATRIC@Day)
  VWC1 <- melt.data.table(VWC1, id.vars = c('Year', 'Day'))

  # Soil Temperature
  #sTemp <- sw_out@SOILTEMP@Day

  # Get EcoVars ----------------------------------------------------------------

  if(calc_EcoVars) {

    # Shriver 2018  Vars
    Shriver2018Vars <- getShriver2018Vars(Temp1, VWC1)

    #OConnor Vars
    #Oconnor2020Vars <- getOConnor2020Vars(sTemp, VWC1, SoilsDF, TimePeriod,
    #                                      currYear, currDate)

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
  VWC1 <-  merge(VWC1, SoilsDF, by = 'variable')
  VWC1 <- VWC1[,.(VWC = weighted.mean(value, width)),
                       .(Year, Day, Depth)]
  VWC1$Depth <- paste0('VWC.', VWC1$Depth)
  VWC1 <- dcast(VWC1, Year + Day ~  Depth, value.var = 'VWC')
  
  if(TimePeriod == 'Future'){
    Data <- cbind(Data, run = nn, run_year = run_year)
  }  

  # Join up
  yy <- dim(VWC1)[2] 
  x1 <- dim(Data)[2]+1
  x2 <- dim(Data)[2] + (yy - 2)
  
  Data[,names(VWC1)[c(3:yy)]] <- as.numeric()
 if(TimePeriod == 'Future'){
   dd <- 4 } 
  else {
     dd <- 2
   }
  

  for(d in x1:x2)  Data[,d] <- VWC1[[(d-dd)]]

  if(!calc_EcoVars) return(list(Data))
  if(calc_EcoVars) return(list(Data, Shriver2018Vars,
                               data.table(GISSM_1[[1]]), 
                               Oconnor2020Vars))

}


#' A function to calculate 30 days rolling mean or sums for multiple variables
#'
#' @param Data2. A data.frame containing daily site-specific results of temperature,
#' precipitation, and volumetric water content.
#' @param TimePeriod character. Future or Historical
#'
#' @return A data.frame.
#'
getRolling <- function(Data2, TimePeriod){
  # get rolling sums and means for ppt and temps
  if(TimePeriod == 'Historical') {
    Data2$ppt_rollsum <- frollsum(Data2[,'ppt'], 30, align = 'center')
    Data2$avg_C_rollmean <- frollmean(Data2[,'avg_C'], 30, align = 'center')
    Data2$VWC.Shallow_rollmean <- frollmean(Data2[,'VWC.Shallow'], 30, align = 'center')
    
    if("VWC.Intermediate" %in% names(Data2)) {
      
      Data2$VWC.Intermediate_rollmean <- frollmean(Data2[,'VWC.Intermediate'], 30, align = 'center')
      
    }
    
    if("VWC.Deep" %in% names(Data2)) {
      
      Data2$VWC.Deep_rollmean <- frollmean(Data2[,'VWC.Deep'], 30, align = 'center')
      
    }
  }
  
  if(TimePeriod == 'Future') {
    Data2$ppt_rollsum <- frollsum(Data2[,'ppt.mean'], 30, align = 'center')
    Data2$avg_C_rollmean <- frollmean(Data2[,'avg_C.mean'], 30, align = 'center')
    Data2$VWC.Shallow_rollmean <- frollmean(Data2[,'VWC.Shallow.mean'], 30, align = 'center')
    
    if(any(c("VWC.Intermediate", "VWC.Intermediate.mean") %in% names(Data2))) {
      
      Data2$VWC.Intermediate_rollmean <- frollmean(Data2[,'VWC.Intermediate.mean'], 30, align = 'center')
      
    }
    
    if(any(c("VWC.Deep", "VWC.Deep.mean") %in% names(Data2))) {
        
      Data2$VWC.Deep_rollmean <- frollmean(Data2[,'VWC.Deep.mean'], 30, align = 'center')
      
    }
    
  }

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
  sand <- as.numeric(SoilsDF[1, 'sand_frac'])/100
  clay <- as.numeric(SoilsDF[1, 'clay_frac'])/100

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
  sTemp_top <- sTempDF[, c('Year', 'Day', 'Lyr_1', 'TP')]
  sTemp_top <- makeDateMonthDat(sTemp_top, 'Day')
  sTemp_top <- sTemp_top[sTemp_top$Date %in% c(paste0('03-0',1:9),paste0('03-',10:31)) ,]

  # VWC mean
  # standard error of SWP needs to be calculated from SWP, not from VWC and converted to SWP
  VWC_top <- VWCDF[variable == 'Lyr_1', ]
  VWC_top <- makeDateMonthDat(VWC_top, 'Day')
  VWC_top <- VWC_top[VWC_top$Date %in% c(paste0('03-0',1:9),paste0('03-',10:31)) ,]

  VWC_top$SWP <- rSOILWAT2::VWCtoSWP(VWC_top$value, sand, clay)
  VWC_top$SWP <- ifelse(VWC_top$SWP < -8, -8, VWC_top$SWP)

  # statistics taken in format step
  vars <- merge(sTemp_top[,c('Date', 'Year', 'TP','Lyr_1')],
                VWC_top[,c('Date', 'Year', 'TP', 'SWP')],  by = c('Year','Date', 'TP'))

  return(vars)

}


