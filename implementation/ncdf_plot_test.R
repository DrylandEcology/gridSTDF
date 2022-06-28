#Objectives: 
# 1) Explore plotting netCDFs 
# 3) Compare OG files to my writes (i.e. Am I getting the indexing correct?)

rm(list = ls(all = TRUE))
library(RNetCDF)

file <- "Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc"

# RNetCDF ----------------------------------------------------------------------
temp.nc <- open.nc(file)
print.nc(temp.nc)

# get
lat <- var.get.nc(temp.nc,"lat")
lon <- var.get.nc(temp.nc,"lon")
data <- var.get.nc(temp.nc,"air_temperature")

# fix latitude! Its reversed
lat <- rev(lat)
data <- data[, ncol(data):1,  ] #lat being our dimension number 2

#plot
data_day1 <- data[, , 1]
image(data_day1)
image(lon,lat,data_day1) 


# FILE I CREATED ---------------------------------------------------------------

file2 <- "test_weather.nc"
temp.nc2 <- open.nc(file2)
print.nc(temp.nc2)


data2 <- var.get.nc(temp.nc2, "MaxTemp")

#plot
data2_day1 <- data2[, , 1]
image(data2_day1)




