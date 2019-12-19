# web interface
library(plumber)
# analysis
library(rSFSW2)
library(rSOILWAT2)
library(splines)
library(raster)
# data formatting
library(data.table)
library(dplyr)
library(lubridate)

functionFiles <- list.files('functions', full.names = TRUE)
sapply(functionFiles, source)
r <- plumb('functions/simulationFunctions.R')
r$run(host = "0.0.0.0",port = 8080)# https://www.r-bloggers.com/hosting-a-plumber-api-on-aws/

#curl 10.12.7.42:8080/gatherDataAndExecuteSW?"lat=35.12&lng=-111.58&soils=2&comp=2"
#curl 127.0.0.1:8000/gatherDataAndExecuteSW?"lat=35.12&lng=-111.58&soils=2&comp=2"
