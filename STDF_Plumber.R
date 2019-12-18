# web interface
library(plumber)
# analysis
library(rSFSW2)
library(rSOILWAT2)
library(splines)
# data formatting
library(data.table)
library(dplyr)
library(lubridate)

functionFiles <- list.files('functions', full.names = TRUE)
sapply(functionFiles, source)
r <- plumb('functions/simulationFunctions.R')
r$run(port = 8000)