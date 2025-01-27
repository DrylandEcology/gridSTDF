# determining the accuracy of GISSM output
# Alice Stears 
# 27 August 2024


# load packages -----------------------------------------------------------

library(tidyverse)
library(RNetCDF)
library(terra)

# Load data ---------------------------------------------------------------

gissm_hist <- terra::rast(x = "./projects/07_TestOutputForFRESC/trimmed_netCDFs/GISSM_yr_gridSTDF_historical_092024.nc")
gissm_pred <- terra::rast(x = "./projects/07_TestOutputForFRESC/trimmed_netCDFs/GISSM_yr_gridSTDF_prediction_092024.nc", drivers="NETCDF")
yr1_rast <- terra::subset(gissm_pred, subset = 1:30) %>% 
  terra::mean()
yr2_rast <- terra::subset(gissm_pred, subset = 31:60) %>% 
  terra::mean() 

png("./projects/07_TestOutputForFRESC/GISSMpredictionFigs.png", width = 700, height = 800, res = 200)

  par(mfrow = c(2,1))
terra::plot(yr1_rast, range = c(0,1), main = "Avg. Sagebrush establishment predicted from \n GISSM/Schlaepfer model in 2024", cex.main = c(.75))
terra::plot(yr2_rast, range = c(0,1), main = "Avg. Sagebrush establishment predicted from \n GISSM/Schlaepfer model in 2025", cex.main = c(.75))

dev.off()
## double check by looking at data as a netCDF rather than a raster
# get the prediction data as a netCDF
gissm_pred_nc <- open.nc("./projects/07_TestOutputForFRESC/trimmed_netCDFs/GISSM_yr_gridSTDF_prediction_092024.nc")
# get the data in the netcdf
test <- var.get.nc(gissm_pred_nc, variable = "probability")#, start = c(194, 435, 1, 1), count = c(10, 1, 30, 2))
# get predictions for year 1
yr1_temp <- test[,,,1]
# average across all 30 simulations
yr1 <- apply(yr1_temp, MARGIN = c(1,2), FUN = function(x)  mean(x))
image(yr1)

# get predictions for year 2
yr2_temp <- test[,,,2]
# average across all 30 simulations
yr2 <- apply(yr2_temp, MARGIN = c(1,2), FUN = function(x)  mean(x))
image(yr2, )

sims <- RNetCDF::var.get.nc(gissm_pred_nc, variable = "simulation")
# get predictions for year 2

RNetCDF::print.nc(gissm_pred_nc)

