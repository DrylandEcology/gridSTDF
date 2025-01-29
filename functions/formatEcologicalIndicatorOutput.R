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
  
  return(raster::bind(Hist_Shriver2018, Future_ShriverMeds))
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

#' Organizes future GISSM output 
#'
#' @param Future_GISSM a data.frame containing outputs from the GISSM
#'  model for the future period.
#'
#' @return data.frame

formatfutureGISSM <- function(Future_GISSM) {
  
  Future_GISSM$run_year <- sapply(strsplit(Future_GISSM$run, '_'), '[', 2)
  Future_GISSM$run_sim <- sapply(strsplit(Future_GISSM$run, '_'), '[', 1)
  Future_GISSM <- Future_GISSM[, .(Prob = mean(SeedlingSurvival_1stSeason)), .(run_sim, ryear)]

  Future_GISSM <- Future_GISSM[order(Future_GISSM$ryear),]
  return(Future_GISSM)
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
  
  # make one dataset
  All <- rbind(Hist_OConnor2020, Future_OConnor2020)
  
  # get stats
  Outs <- setDT(All)[, .(sTemp_mean = mean(Lyr_1_avg_C),
                         sTemp_CI95 = std(Lyr_1_avg_C * 1.96),
                         SWP_mean = mean(SWP),
                         SWP_CI95 = std(SWP) * 1.96),
                     .(Date, TP)]
  
  return(Outs)
}
