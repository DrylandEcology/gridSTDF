rm(list=ls(all = TRUE))
#library(rgdal)
library(terra)
library(lubridate)
#  -of COG -co BLOCKSIZE=512 -co RESAMPLING=BILINEAR -co COMPRESS=DEFLATE -co NUM-THREADS=25 -co BIGTIFF=YES

# need to decide which year of ecological variable are being shown (show 
# current year until a certain point, then switch to the next year)--just need to 
# document clearly which data is being used Blue text is actual list of COGs 
# (rather than text that describes what we want) (green text indicates names of 
# netCDFs to calculate variables required) 


# Get date information and prep for loading files ----------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

## get the location of netCDFs output from this model run
fileLoc <-  paste0('./outputs/', format(currDate, "%Y%m%d"))
## get the location for saving the COGs output from this model run
outLoc <- paste0("./outputs/", format(currDate,  "%Y%m%d"),"/COGs/")
if(!dir.exists(outLoc)) {
  dir.create(paste0("./outputs/", format(currDate,  "%Y%m%d"),"/COGs/"))
}

# Shriver model: Median of values across the historical period -------------------------------------------------------
# (average of values in each cell of shriver_yr_gridSTDF_historical.nc)
# load data
shriver_hist_probs <- rast(paste0(fileLoc, "shriver_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc"))
# calculate medians
shriver_hist_meds <- median(shriver_hist_probs, na.rm = TRUE)
# get the information from the time axis 
shriver_hist_time <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "shriver_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)
# save the median data as a COG
terra::writeRaster(shriver_hist_meds, filename = paste0(outLoc,"ShriverHistoricalPreds_medians_from_", shriver_hist_time[1],"_to_",shriver_hist_time[length(shriver_hist_time)],".tif"), gdal = "COG")

# Shriver model: Probability of establishment for the next year relative to median values ---------------------------------------------------------
# (median calculated from shriver_yr_gridSTDF_historical.nc;
# probability for this year calculated from shriver_yr_gridSTDF_prediction.nc
# (average of all 30 values for a year)) (need to have a cutoff month prior to
# which we calculate the average probability for the current year, and after
# which we calculate the average probability for the next year)

shriver_pred_probs <- rast(paste0(fileLoc, "shriver_yr_gridSTDF_prediction_", format(currDate, "%m%Y"), ".nc"))
# get the median of predictions for the current year 
shriver_predMedian_currentYear <- median(subset(shriver_pred_probs, sub = 1:30))
# get the median of predictions for the next year 
shriver_predMedian_nextYear <- median(subset(shriver_pred_probs, sub = 31:60))
# get the time data for this layer
shriver_hist_time <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "shriver_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)

# calculate the anomalies for each real relative to the historical median (calculated above)
shriver_predAnom_currentYear <- shriver_predMedian_currentYear - shriver_hist_meds 
shriver_predAnom_nextYear <- shriver_predMedian_nextYear - shriver_hist_meds

# save the prediction data as a COG
terra::writeRaster(shriver_predAnom_currentYear, 
                   filename = paste0(outLoc,"ShriverPredictedMedianRelativeToHistoricalData_for_",shriver_hist_time[1],".tif"), gdal = "COG")
terra::writeRaster(shriver_predAnom_nextYear, 
                   filename = paste0(outLoc,"ShriverPredictedMedianRelativeToHistoricalData_for_",shriver_hist_time[2],".tif"), gdal = "COG")

# OConnor: Predicted # of days in March w/ SWP > -2.5MPa & soil temp > 0 C  -----------------------------------------------------------------
# (get from oconnor-swp_dy_gridSTDF_mean-prediction.nc and oconnor-soiltemp_dy_gridSTDF_mean-prediction.nc files) 
# get SWP data
OConnor_swp <- rast(paste0(fileLoc, "oconnor-swp_dy_gridSTDF_mean-prediction_", format(currDate, "%m%Y"), ".nc"))
# each layer is a day in March 
# find the IDs of days in a cell where SWP is < -2.5MPa 
OConnor_swp_goodIDs <- terra::app(OConnor_swp, 
                              function(x) x>-2.5)

# get temp data
OConnor_temp <- rast(paste0(fileLoc, "oconnor-soiltemp_dy_gridSTDF_mean-prediction_", format(currDate, "%m%Y"), ".nc"))
# each layer is a day in March
# find the IDs of days in a cell where temp > 0 C
OConnor_temp_goodIDs <- terra::app(OConnor_temp, 
                                  function(x) x>0)

# find days where soil is moist enough and temp is high enough 
goodDays <- OConnor_swp_goodIDs * OConnor_temp_goodIDs
#plot(OConnor_swp_goodIDs)
#plot(OConnor_temp_goodIDs) 
#plot(goodDays)
# find the total number of days w/in each cell 
goodDaysPerCell <- sum(goodDays)
#plot(goodDaysPerCell)
# get the year that the "march" values correspond to 
OConnor_year <- min(1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "oconnor-soiltemp_dy_gridSTDF_mean-prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365))

# save the prediction data as a COG
terra::writeRaster(goodDaysPerCell, 
                   filename = paste0(outLoc,"OConnorPredicted_NumberOfMoistAndWarmDays_March_",OConnor_year,".tif"), gdal = "COG")


#Idea: could we somehow indicate which cells have values for these variables
#whose CIs don’t overlap with the threshold values? (i.e. when the predicted
#values overlap with the blue line from the figure, are substantially above it,
#or substantially below it)

#OConnor: Predicted of days soil MPa was between 0.5 and 0 in March --------------------------------------------------------------- 
#(get from oconnor-swp_dy_gridSTDF_mean-prediction.nc file) # both for for
#current year prior to cutoff month; for next year after cutoff month (?) #
## the year for data shown is indicated in the file name 
# get SWP data (from above)
# each layer is a day in March 
# find the IDs of days in a cell where SWP is between -.5 and 0
OConnor_swp_goodIDs_2 <- terra::app(OConnor_swp, 
                                  function(x) x>-.5 & x<0)

# get the total number of days with the conditions we want 
goodDaysPerCell_2 <- sum(OConnor_swp_goodIDs_2)
#plot(goodDaysPerCell)
# get the year that the "march" values correspond to 
OConnor_year <- min(1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "oconnor-soiltemp_dy_gridSTDF_mean-prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365))

# save the prediction data as a COG
terra::writeRaster(goodDaysPerCell, 
                   filename = paste0(outLoc,"OConnorPredicted_NumberOfVeryMoistDays_March_",OConnor_year,".tif"), gdal = "COG")

# GISSM: Mean of values across the historical period -----------------------------------------------------------------
# (average of values in each cell of GISSM_yr_gridSTDF_historical.nc) 
# load data
GISSM_hist_probs <- rast(paste0(fileLoc, "GISSM_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc"))
# calculate means
GISSM_hist_means <- mean(GISSM_hist_probs, na.rm = TRUE)
# get the information from the time axis 
GISSM_hist_time <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "GISSM_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)
# save the median data as a COG
terra::writeRaster(GISSM_hist_means, filename = paste0(outLoc,"GISSM_HistoricalPreds_means_from_", GISSM_hist_time[1],"_to_",GISSM_hist_time[length(shriver_hist_time)],".tif"), gdal = "COG")

# GISSM: Probability of seedling survival for the next year relative to median values -----------------------------------------------------------------

# Probability of seedling survival for the next year relative to median values
# (median calculated from GISSM_yr_gridSTDF_historical.nc; probability for this
# year calculated from GISSM_yr_gridSTDF_prediction.nc (average of all 30 values
# for a year)) (need to have a cutoff month prior to which we calculate the
# average probability for the current year, and after which we calculate the
# average probability for the next year)
# load data
GISSM_preds <- rast(paste0(fileLoc, "GISSM_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc"))

## get data for year 1
GISSM_preds_1 <- terra::subset(GISSM_preds, 1:30)
# calculate medians
GISSM_preds_mean_1 <- mean(GISSM_preds_1, na.rm = TRUE)
# get the information from the time axis 
GISSM_preds_time_1 <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "GISSM_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)[1]
# compare the predictions to the historical values 
GISSM_preds_anoms_1 <- GISSM_preds_mean_1 - GISSM_hist_means
# save the median data as a COG
terra::writeRaster(GISSM_preds_anoms_1, 
                   filename = paste0(outLoc,"GISSM_MeanPredictedAnomaliesRelativeToHistoricalMeans_from_", GISSM_preds_time_1,".tif"), gdal = "COG")

## get data for year 2
GISSM_preds_2 <- terra::subset(GISSM_preds, 31:60)
# calculate medians
GISSM_preds_mean_2 <- mean(GISSM_preds_2, na.rm = TRUE)
# get the information from the time axis 
GISSM_preds_time_2 <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "GISSM_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)[2]
# compare the predictions to the historical values 
GISSM_preds_anoms_2 <- GISSM_preds_mean_2 - GISSM_hist_means
# save the median data as a COG
terra::writeRaster(GISSM_preds_anoms_2, 
                   filename = paste0(outLoc,"GISSM_MeanPredictedAnomaliesRelativeToHistoricalMeans_from_", GISSM_preds_time_2,".tif"), gdal = "COG")

# Soil Moisture: Mean predicted surface (?) soil moisture for growing season -----------------------------------------------------------
# (define differently for different regions?) (from vwc-shallow_dy_gridSTDF_median-prediction.nc) 


# Soil Moisture: Deltas (comparison of mean to normal period for the same period) for mean surface (?) soil moisture for growing season --------------------------------------------------------------------
# (define differently for different regions?) (from vwc-shallow_dy_gridSTDF_median-diffs-prediction.nc) 


# Soil Moisture: Mean surface (?) soil moisture for Fall (?)-------------------------------------------------------------
# (from vwc-shallow_dy_gridSTDF_median-prediction.nc) 


# Soil Moisture: Deltas for mean surface (?) soil moisture for Fall  (?) ----------------------------------------------------------
# (from vwc-shallow_dy_gridSTDF_median-diffs-prediction.nc) 

## could maybe use some sort of stippling/shading to indicate when predictions
## are “significantly different” from the normal period (i.e. CIs don’t
## overlap)--especially for deltas


# Precip: Mean predicted precip values over the next three months--------------------------------------------------------------------

# (or could do growing season... depends on what we think this information would
# be useful for) (from ppt_dy_gridSTDF_median-prediction.nc)


# Precip: Deltas for precip over the next three months  -----------------------------------------------------------------
# (comparison of mean to normal period for the same period) (from ppt_dy_gridSTDF_median-diffs-prediction.nc) 


# Temp: Mean predicted temp values over the next three months ----------------------------------------------------------------
# (from tmean_dy_gridSTDF_median-prediction.nc) 


# Temp: Deltas for temp over the next three months ------------------------------------------------------------------
# (comparison of mean to normal period for the same period) (from tmean_dy_gridSTDF_median-diffs-prediction.nc) 

###Idea: could use some sort of stippling/shading to indicate when predictions are “significantly different” from the normal period (i.e. CIs don’t overlap)--especially for deltas 
