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
  sTemp <- data.table(sw_out@SOILTEMP@Day)

  # Get EcoVars ----------------------------------------------------------------

  if(calc_EcoVars) {

    # Shriver 2018  Vars
    Shriver2018Vars <- getShriver2018Vars(Data, VWC1)

    #OConnor Vars
    #Oconnor2020Vars <- getOConnor2020Vars(sTemp, VWC1, SoilsDF, TimePeriod,
    #                                     currYear, currDate)

    GISSM_1 <- suppressWarnings(rSW2funs::calc_GISSM(
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
  xstart <- dim(Data)[2]+1
  xend <- dim(Data)[2] + (yy - 2)
  
  Data[,names(VWC1)[c(3:yy)]] <- as.numeric()
 
  if(TimePeriod == 'Future') {
    dd <- 4 
   } else {
    dd <- 2
   }
  
  for(d in xstart:xend)  Data[,d] <- VWC1[[(d-dd)]]

  if(!calc_EcoVars) return(list(Data))
  if(calc_EcoVars) return(list(Data, 
                               Shriver2018Vars,
                               data.table(GISSM_1[[1]])))
                               #Oconnor2020Vars))

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

