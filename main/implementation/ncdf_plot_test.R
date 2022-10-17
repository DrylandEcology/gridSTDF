#Objectives: 
# 1) Explore plotting netCDFs 
# 3) Compare OG files to my writes (i.e. Am I getting the indexing correct?)

rm(list = ls(all = TRUE))
library(stars)
library(RNetCDF)

file1 <- "main/Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc"
file2 <- "~/Desktop/test_maxtemp.nc"


# RNetCDF ----------------------------------------------------------------------
temp.nc1 <- open.nc(file1)
temp.nc2 <- open.nc(file2)
print.nc(temp.nc11)
print.nc(temp.nc2)


# get
lat <- var.get.nc(temp.nc,"Latitude")
lon <- var.get.nc(temp.nc,"Longitude")
data1 <- var.get.nc(temp.nc1, "air_temperature")
data2 <- var.get.nc(temp.nc2,"testMatrix")
summary(data1)

# fix latitude! Its reversed
lat <- rev(lat)
data <- data[, ncol(data):1,  ] #lat being our dimension number 2

#plot
data1_day1 <- data1[, , 1]
image(data1_day1)
data2_day1 <- data2[, , 1]
image(data2_day1)

image(lon,lat,data_day1) 


# stars
temp.nc1 <- read_ncdf(file1)
temp.nc2 <- read_ncdf(file2)

#  
library(dplyr)
prec_slice = slice(temp.nc1, index = 1, along = "day")
plot(prec_slice, border = NA, breaks = qu_0_omit(prec_slice[[1]]), reset = FALSE)


