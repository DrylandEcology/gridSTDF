#Objectives: 
# 1) Explore plotting netCDFs 
# 3) Compare OG files to my writes (i.e. Am I getting the indexing correct?)

#rm(list = ls(all = TRUE))
library(stars)
library(RNetCDF)
#devtools::install_github("r4ecology/rcdo", dependencies = TRUE, force = TRUE)
library(terra)


# RNetCDF ----------------------------------------------------------------------
example1 <- 'projects/06-TestOutputs/swp-med_dy_gridSTDF_historical_19910101-20201231-10pct_002023.nc'
example1 <- 'projects/06-TestOutputs/vwc-med_dy_gridSTDF_median-prediction_002023.nc'
example1 <- 'projects/03-Make-Climatologies-netCDF/ta_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231.nc'

# temp.nc1 <- terra::rast(example1)
# plot(temp.nc1[[1]])
# plot(temp.nc1[[2]])

temp.nc1 <- open.nc(example1)
print.nc(temp.nc1)

# get
#lat <- var.get.nc(temp.nc1,"lat")
#lon <- var.get.nc(temp.nc1,"lon")

data1 <- var.get.nc(temp.nc1, "vwc")
data1_day1 <- data1[, , 1]
image(data1_day1)





# # stars
# temp.nc1 <- read_ncdf(file1)
# temp.nc2 <- read_ncdf(file2)
# 
# #  
# library(dplyr)
# t_slice1 = slice(temp.nc1, index = 1, along = "day")
# plot(t_slice1, border = NA, breaks = qu_0_omit(t_slice1[[1]]), reset = FALSE)
# 
# t_slice1 = slice(temp.nc1, index = 1, along = "day")
# plot(t_slice1, border = NA, breaks = qu_0_omit(t_slice1[[1]]), reset = FALSE)
# 
# 
