rm(list = ls(all = TRUE))
library(stars)
library(RNetCDF)
#devtools::install_github("r4ecology/rcdo", dependencies = TRUE, force = TRUE)
library(rcdo)

file1 <- "projects/03-Make-Climatologies-netCDF/Outputs/pr_daily_gridSTDF_historical_19910101-20201231-clim_102022.nc"
file1 <- "projects/03-Make-Climatologies-netCDF/Outputs/tmmn_daily_gridSTDF_historical_19910101-20201231_clim_102022.nc"
file1 <- "projects/03-Make-Climatologies-netCDF/Outputs/tmmx_daily_gridSTDF_historical_19910101-20201231-clim_102022.nc"

temp.nc1 <- open.nc(file1)
print.nc(temp.nc1)

# get
lat1 <- var.get.nc(temp.nc1,"lat")
lon2 <- var.get.nc(temp.nc1,"lon")
data1 <- var.get.nc(temp.nc1, "pr")

#plot
data1_day1 <- data1[, , 1]
image(data1_day1)
