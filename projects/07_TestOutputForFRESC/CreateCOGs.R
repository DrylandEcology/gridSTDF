rm(list=ls(all = TRUE))
#library(rgdal)
library(terra)
library(lubridate)
library(RNetCDF)

# Get date information and prep for loading files ----------------------------------------------------
currDOY <- lubridate::yday(Sys.Date())
currMonth <- lubridate::month(Sys.Date())
currYear <- lubridate::year(Sys.Date())
currDate <- Sys.Date()
todayMonthDay <- format(Sys.Date() , format="%m-%d")

## get the location of netCDFs output from this model run
fileLoc <-  paste0('./outputs/', format(currDate, "%Y%m%d"))

# Get Dates For Current Test Iteration -----------------------------------------
currDOY <-  lubridate::yday("2024-09-03")
currMonth <- lubridate::month("2024-09-03")
currYear <- lubridate::year("2024-09-03")
currDate <- as.Date("2024-09-03")
todayMonthDay <- format("2024-09-03" , format="%m-%d")
fileLoc <- "./projects/07_TestOutputForFRESC/trimmed_netCDFs/" 

## get the location for saving the COGs output from this model run
outLoc <- paste0("./outputs/", format(currDate,  "%Y%m%d"),"/COGs/")
if(!dir.exists(outLoc)) {
  dir.create(paste0("./outputs/", format(currDate,  "%Y%m%d"),"/COGs/"))
}

# Shriver model: Median of values across the historical period -------------------------------------------------------
# (average of values in each cell of shriver_yr_gridSTDF_historical.nc)
# load data
shriver_hist_probs <- rast(paste0(fileLoc, "shriver_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc"))
# get the information from the time axis 
shriver_hist_time <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "shriver_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)
# subset for the values just from the normal period (1991 to 2020, so recruitment in 1992-2021)
shriver_hist_probsNorm <- subset(shriver_hist_probs, 1:30)
# calculate medians
shriver_hist_meds <- median(shriver_hist_probsNorm, na.rm = TRUE)
# save the median data as a COG
terra::writeRaster(shriver_hist_meds, filename = paste0(outLoc,"shriver_historical_medianOfHindscastsFrom1991to2020.tif"), gdal = "COG")

# Shriver model: Probability of establishment for the next year---------------------------------------------------------
# Note that this is just straight median of predictions, not the anomaly!
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

# save the prediction data as a COG
terra::writeRaster(shriver_predMedian_currentYear, 
                   filename = paste0(outLoc,"shriver_prediction_medianOfPredictionsFor",shriver_hist_time[1],".tif"), gdal = "COG")
terra::writeRaster(shriver_predMedian_nextYear, 
                   filename = paste0(outLoc,"shriver_prediction_medianOfPredictionsFor",shriver_hist_time[2],".tif"), gdal = "COG")

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
                   filename = paste0(outLoc,"oconnor_predicted_NumberOfMoistAndWarmDays_March_",OConnor_year,".tif"), gdal = "COG")


#Idea: could we somehow indicate which cells have values for these variables
#whose CIs don’t overlap with the threshold values? (i.e. when the predicted
#values overlap with the blue line from the figure, are substantially above it,
#or substantially below it)

#OConnor: Predicted of days soil MPa was between -0.5 and 0 in March --------------------------------------------------------------- 
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
terra::writeRaster(goodDaysPerCell_2, 
                   filename = paste0(outLoc,"oconnor_predicted_NumberOfVeryMoistDays_March_",OConnor_year,".tif"), gdal = "COG")

# GISSM: Mean of values across the historical period -----------------------------------------------------------------
# (average of values in each cell of GISSM_yr_gridSTDF_historical.nc) 
# load data
GISSM_hist_probs <- rast(paste0(fileLoc, "GISSM_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc"))
# get the information from the time axis 
GISSM_hist_time <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "GISSM_yr_gridSTDF_historical_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)
# get the values just for the normal period (1991 - 2020, which is 1992-2021 for the GISSM output)
GISSM_hist_probsNorm <- subset(GISSM_hist_probs, which(GISSM_hist_time %in% c(1992:2021)))
# calculate means
GISSM_hist_means <- mean(GISSM_hist_probsNorm, na.rm = TRUE)

# save the mean data as a COG
terra::writeRaster(GISSM_hist_means, filename = paste0(outLoc,"GISSM_historical_meanOfHindscastsFrom", GISSM_hist_time[1]-1,"to",GISSM_hist_time[30]-1,".tif"), gdal = "COG", overwrite = TRUE)

# GISSM: Mean Predicted probability of seedling survival for the next year-----------------------------------------------------------------
# Note: this is teh true mean, not the anomaly! 
# Probability of seedling survival for the next year relative to median
# historical values (median calculated from GISSM_yr_gridSTDF_historical.nc;
# probability for this year calculated from GISSM_yr_gridSTDF_prediction.nc
# (average of all 30 values for a year)) (need to have a cutoff month prior to
# which we calculate the average probability for the current year, and after
# which we calculate the average probability for the next year) load data
GISSM_preds <- rast(paste0(fileLoc, "GISSM_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc"))

## get data for year 1
GISSM_preds_1 <- terra::subset(GISSM_preds, 1:30)
# calculate mean
GISSM_preds_mean_1 <- mean(GISSM_preds_1, na.rm = TRUE)
# get the information from the time axis 
GISSM_preds_time_1 <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "GISSM_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)[1]

# save the median data as a COG
terra::writeRaster(GISSM_preds_mean_1, 
                   filename = paste0(outLoc,"GISSM_prediction_meanOfPredictionsFor", GISSM_preds_time_1,".tif"), gdal = "COG")

## get data for year 2
GISSM_preds_2 <- terra::subset(GISSM_preds, 31:60)
# calculate means
GISSM_preds_mean_2 <- mean(GISSM_preds_2, na.rm = TRUE)
# get the information from the time axis 
GISSM_preds_time_2 <- 1970 + round(var.get.nc(ncfile = open.nc(paste0(fileLoc, "GISSM_yr_gridSTDF_prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")/365)[2]
# save the means data as a COG
terra::writeRaster(GISSM_preds_mean_2, 
                   filename = paste0(outLoc,"GISSM_prediction_meanOfPredictionsFor", GISSM_preds_time_2,".tif"), gdal = "COG")


# Precip: Median predicted precip values over the next three months--------------------------------------------------------------------
# (or could do growing season... depends on what we think this information would
# be useful for) (from ppt_dy_gridSTDF_median-prediction.nc)
## currently just the next 3 months 
# get data
precip_preds<- rast(paste0(fileLoc, "ppt_dy_gridSTDF_median-prediction_",  format(currDate, "%m%Y"), ".nc"))

# get the information from the time axis 
precip_pred_time <- var.get.nc(ncfile = open.nc(paste0(fileLoc, "ppt_dy_gridSTDF_median-prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")
# calculate the dates (were previously # of days since 1-1-1970)
precip_pred_time <- lubridate::as_date(precip_pred_time, origin = "1970-01-01")

# get the indices of values that are within the next three months (next 90 days)
goodDates <- which(precip_pred_time %in% as_date(c(currDate:(currDate+90))))

## get data for the next three months
precip_pred_90days <- terra::subset(precip_preds, goodDates)
#precip_pred_mean_90days <- mean(precip_pred_90days, na.rm = TRUE)
precip_pred_median_90days <- median(precip_pred_90days, na.rm = TRUE)

# save the mean data as a COG
terra::writeRaster(precip_pred_median_90days, filename = paste0(outLoc,
                                                                "ppt_median-prediction_MedianOfMedianDailyValuesOverNext90Days_from", precip_pred_time[goodDates][1],"_to_",currDate+90, ".tif"), gdal = "COG", overwrite = TRUE)

# Precip: Deltas for precip over the next three months  -----------------------------------------------------------------
# (comparison of mean to normal period for the same period) (from ppt_dy_gridSTDF_median-diffs-prediction.nc) 
## currently just the next 3 months 
# get data
#precip_deltas <- rast(paste0(fileLoc, "ppt_dy_gridSTDF_median-diffs-prediction_",  format(currDate, "%m%Y"), ".nc"))
precip_hist <- rast(paste0(fileLoc, "ppt_dy_gridSTDF_historical_19910101-20201231-median_",  format(currDate, "%m%Y"), ".nc"))
# get the information from the time axis 
precip_hist_time <- var.get.nc(ncfile = open.nc(paste0(fileLoc, "ppt_dy_gridSTDF_historical_19910101-20201231-median_",  format(currDate, "%m%Y"), ".nc")), variable = "time")
# calculate the dates (were previously # of days since 1-1-1970)
precip_hist_time <- lubridate::as_date(precip_hist_time, origin = "1970-01-01")

# get the indices of values that are within the next three months (next 90 days)
goodDates_preds <- which(precip_pred_time %in% as_date(c(currDate:(currDate+90))))
goodDates_hist <- which(precip_hist_time  %in%  as_date(c(precip_pred_time[1]:(currDate+90))))
# make sure the range of dates is the same
sum(precip_pred_time[goodDates_preds] != precip_hist_time[goodDates_hist]) # should be zero
length(goodDates_preds) == length(goodDates_hist) # should be true 

## get data for the next three months
# for prediction data
precip_pred_90days <- terra::subset(precip_preds, goodDates_preds)
# for historical data 
precip_hist_90days <- terra::subset(precip_hist, goodDates_hist)
# calculate diffs between each day (prediction - normal)
diffList <- lapply(X = 1:length(goodDates_preds), FUN = function(x) {
  temp <- subset(precip_pred_90days, x) - subset(precip_hist_90days, x)
  return(temp)
})
diffs <- rast(diffList)
# average across all days to get the mean diff over the next 90 days 
precip_diff_means <- mean(diffs, na.rm = TRUE)
#precip_diff_medians <- median(diffs, na.rm = TRUE)
# save the mean data as a COG
terra::writeRaster(precip_diff_means, filename = paste0(outLoc,"ppt_median-diffs-prediction_MeanOfMedianDailyDiffsOverNext90Days_from", precip_pred_time[goodDates_preds][1],"_to_",currDate+90, ".tif"), gdal = "COG", overwrite = TRUE)

# Temp: Mean predicted temp values over the next three months ----------------------------------------------------------------
# (from tmean_dy_gridSTDF_median-prediction.nc) 
## currently just the next 3 months 
# get data
temp_preds<- rast(paste0(fileLoc, "tmean_dy_gridSTDF_median-prediction_",  format(currDate, "%m%Y"), ".nc"))

# get the information from the time axis 
temp_pred_time <- var.get.nc(ncfile = open.nc(paste0(fileLoc, "tmean_dy_gridSTDF_median-prediction_",  format(currDate, "%m%Y"), ".nc")), variable = "time")
# calculate the dates (were previously # of days since 1-1-1970)
temp_pred_time <- lubridate::as_date(temp_pred_time, origin = "1970-01-01")

# get the indices of values that are within the next three months (next 90 days)
goodDates_temp <- which(temp_pred_time %in% as_date(c(currDate:(currDate+90))))

## get data for the next three months
temp_pred_90days <- terra::subset(temp_preds, goodDates_temp)
temp_pred_mean_90days <- mean(temp_pred_90days, na.rm = TRUE)

# save the mean data as a COG
terra::writeRaster(temp_pred_mean_90days, filename = paste0(outLoc,"tmean_median-prediction_MeanOfDailyMedianTempOverNext90Days_from", temp_pred_time[goodDates][1],"_to_",currDate+90, ".tif"), gdal = "COG", overwrite = TRUE)

# Temp: Deltas for temp over the next three months ------------------------------------------------------------------
# (comparison of mean to normal period for the same period) (from tmean_dy_gridSTDF_median-diffs-prediction.nc) 
# get data
temp_hist <- rast(paste0(fileLoc, "tmean_dy_gridSTDF_historical_19910101-20201231-median_",  format(currDate, "%m%Y"), ".nc"))
# get the information from the time axis 
temp_hist_time <- var.get.nc(ncfile = open.nc(paste0(fileLoc, "tmean_dy_gridSTDF_historical_19910101-20201231-median_",  format(currDate, "%m%Y"), ".nc")), variable = "time")
# calculate the dates (were previously # of days since 1-1-1970)
temp_hist_time <- lubridate::as_date(temp_hist_time, origin = "1970-01-01")

# get the indices of values that are within the next three months (next 90 days)
goodDates_preds <- which(temp_pred_time %in% as_date(c(currDate:(currDate+90))))
goodDates_hist <- which(temp_hist_time  %in%  as_date(c(temp_pred_time[1]:(currDate+90))))
# make sure the range of dates is the same
sum(temp_pred_time[goodDates_preds] != temp_hist_time[goodDates_hist]) # should be zero
length(goodDates_preds) == length(goodDates_hist) # should be true 

## get data for the next three months
# for prediction data
temp_pred_90days <- terra::subset(temp_preds, goodDates_preds)
# for historical data 
temp_hist_90days <- terra::subset(temp_hist, goodDates_hist)
# calculate diffs between each day (prediction - normal)
diffList <- lapply(X = 1:length(goodDates_preds), FUN = function(x) {
  temp <- subset(temp_pred_90days, x) - subset(temp_hist_90days, x)
  return(temp)
})
diffs <- rast(diffList)
# average across all days to get the mean diff over the next 90 days 
temp_diff_means <- mean(diffs, na.rm = TRUE)
# save the mean data as a COG
terra::writeRaster(temp_diff_means, filename = paste0(outLoc,"tmean_median-diffs-prediction_MeanOfDailyDiffOVerNext90Days_from", temp_pred_time[goodDates_preds][1],"_to_",currDate+90, ".tif"), gdal = "COG", overwrite = TRUE)

###Idea: could use some sort of stippling/shading to indicate when predictions are “significantly different” from the normal period (i.e. CIs don’t overlap)--especially for deltas 

