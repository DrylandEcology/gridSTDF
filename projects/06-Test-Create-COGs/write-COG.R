rm(list=ls(all = TRUE))
library(rgdal)
library(terra)

#  -of COG -co BLOCKSIZE=512 -co RESAMPLING=BILINEAR -co COMPRESS=DEFLATE -co NUM-THREADS=25 -co BIGTIFF=YES

example1 <- 'projects/06-TestOutputs/vwc-med_dy_gridSTDF_median-prediction_002023.nc'
temp.nc1 <- terra::rast(example1)
plot(temp.nc1[[1]])

# get one layer object
newFile <- temp.nc1[[1]]
terra::writeRaster(newFile, 'projects/07-Test-Create-COGs/test.tif', gdal="of=COG")


newCOG <- terra::rast('projects/07-Test-Create-COGs/test.tif')
plot(newCOG)


