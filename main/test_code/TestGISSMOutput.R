# determining the accuracy of GISSM output
# Alice Stears 
# 27 August 2024


# load packages -----------------------------------------------------------

library(tidyverse)
library(RNetCDF)
library(terra)

# Load data ---------------------------------------------------------------

gissm_hist <- terra::rast(x = "./projects/07_TestOutputForFRESC/trimmed_netCDFs/GISSM_yr_gridSTDF_historical_042024.nc")
gissm_pred <- terra::rast(x = "./outputs/20240827/GISSM_yr_gridSTDF_prediction_082024.nc", drivers="NETCDF")

# get the prediction data as a netCDF
gissm_pred_nc <- open.nc("./outputs/20240827/GISSM_yr_gridSTDF_prediction_082024.nc")
# get the data in the netcdf
test <- var.get.nc(gissm_pred_nc, variable = "probability")#, start = c(194, 435, 1, 1), count = c(10, 1, 30, 2))

RNetCDF::print.nc(gissm_pred_nc)
