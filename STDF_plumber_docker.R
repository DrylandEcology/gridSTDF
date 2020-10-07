# web interface
library(rSOILWAT2)
library(rSW2funs)
# data formatting
library(data.table)
library(lubridate)
library(raster)
library(zoo)
library(caTools)
library(plyr)
# app
library(rvest)

setwd('/usr/local/app/STDF/shorttermdroughtforecaster') # if on AWS

source('functions/weatherFunctions.R')
source('functions/soilsAndComp.R')
source('functions/Outputs.R')
source('functions/HelperFunctions.R')
source('functions/ecoIndicators.R')


source('functions/simulationFunctions.R')
r <- plumb('functions/simulationFunctions.R')
r$run(host = "0.0.0.0",port = 8080)

# https://www.r-bloggers.com/hosting-a-plumber-api-on-aws/
# curl "http://10.12.7.56:8080/gatherDataAndExecuteSW?clay=15&sand=50&soils=2&lng=-111.58&lat=35.258" > data.json
# curl http://127.0.0.1:8080/gatherDataAndExecuteSW?"lat=43.3737&lng=-111.588&soils=2&sand=50&clay=15" > data3.json
