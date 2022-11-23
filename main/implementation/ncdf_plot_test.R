#Objectives: 
# 1) Explore plotting netCDFs 
# 3) Compare OG files to my writes (i.e. Am I getting the indexing correct?)

rm(list = ls(all = TRUE))
library(stars)
library(RNetCDF)
#devtools::install_github("r4ecology/rcdo", dependencies = TRUE, force = TRUE)
library(rcdo)
library(terra)

file1 <- "main/Data/www.northwestknowledge.net/metdata/data/tmmx_2022.nc"
file2 <- "~/Desktop/test_maxtemp2.nc"

# Clip and write file1
nc_clip(file2,
        lon_range = c(-125, -94.0 ),
        lat_range = c(25, 49.50000),
        out_file = 'west.temp.nc2' ,
        overwrite = TRUE,
        cdo_output = TRUE)



# RNetCDF ----------------------------------------------------------------------
example1 <- '~/Downloads/Air_temperature/ta_yr_SOILWAT2_RangeDroughtExposure_median_RCP45_gn_20210101-21001231-clim.nc'

temp.nc1 <- open.nc(example1)
temp.nc1 <- terra::rast(example1)
plot(temp.nc1[[1]])
plot(temp.nc1[[2]])
temp.nc2 <- open.nc("west.temp.nc2")
print.nc(temp.nc1)
print.nc(temp.nc2)


# get
lat1 <- var.get.nc(temp.nc1,"lat")
lat2 <- var.get.nc(temp.nc2,"lat")
lon <- var.get.nc(temp.nc1,"lon")
lon <- var.get.nc(temp.nc2,"lon")

data1 <- var.get.nc(temp.nc1, "ta")
data2 <- var.get.nc(temp.nc2,"tmmx")
#summary(data1)
#summary(data2)

# fix latitude! Its reversed
lat1 <- rev(lat1)
data1 <- data1[, ncol(data1):1,  ] #lat being our dimension number 2

#plot
data1_day1 <- data1[, , 1]
image(data1_day1)
data2_day1 <- data2[, , 1]
image(data2_day1)

# Scatterplot diagnosis
data1_day1 <- c(data1_day1[!is.na(data1_day1)])
data1_day1 <- data1_day1 - 220
data1_day1 <- data1_day1[1:295991]
summary(data1_day1)
data2_day1 <- c(data2_day1[!is.na(data2_day1)]) # 295991
data2_day1 <- data2_day1 + 273.15
summary(data2_day1)

plot(data1_day1, data2_day1 )



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
