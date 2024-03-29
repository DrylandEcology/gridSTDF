# Get outputs for two sites (test)
# 7 March 2024

#module load netcdf-c/4.9.0
#module load R/4.2.0
#module load geos/3.8.1 gdal/3.0.4 proj/6.2.1 
#R
library(stars)
library(RNetCDF)
library(ncmeta)
library(plyr)

attributes <- read.csv('./main/implementation/nc_atts-all.csv')

outputPath <- "./outputs/20240308"

Outputs <- list.files(outputPath)
TempOuts <- grep('tmean|tmmx', Outputs, value = TRUE) # should get nine variables as it stands
VWCOuts <- grep('vwc-med',  Outputs, value = TRUE)

Outs <- Outputs

# lat <- 35.1983
# long <- -111.6513

# Get indices once ... will always be the same across out netCDF files ---------
allData <- open.nc(file.path(outputPath, Outs[1]))
allLat <- var.get.nc(allData, "lat")
allLon <- var.get.nc(allData, "lon")
close.nc(allData)

# read in data about "Sites" (grid cells)
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))

# make a list to hold data for each site 
siteDat <- vector(mode = "list", length = 2)# nrow(Sites))
# get data for each grid cell in turn 
for (i in 1:2){#nrow(Sites)) { 
  # get the ith lat
  lat_n <- Sites[i,"Latitude"]
  latInd <- Sites[i, "LatIndex"]
  # get the ith long
  long_n <- Sites[i, "Longitude"]
  longInd <- Sites[i, "LonIndex"]

  # set up a data_frame for this grid cell's data
  DataOneSite <- as.data.frame(matrix(nrow = 549, ncol = 100))
 
  EcoVarHistOneSite <- as.data.frame(matrix(nrow = 32, ncol = 3 ))
  names(EcoVarHistOneSite) <- c("Year", "GISSM_yr_gridSTDF_historical_032024.nc", "shriver_yr_gridSTDF_historical_032024.nc")
  EcoVarHistOneSite$Year <- 1990:2021
  
  EcoVarPredOneSite <- as.data.frame(matrix(nrow = 60, ncol = 6))
  names(EcoVarPredOneSite) <- c("Year_GISSM", "Year_Shriver", "Sim_GISSM", "Sim_Shriver", "GISSM_yr_gridSTDF_prediction_032024.nc", "shriver_yr_gridSTDF_prediction_032024.nc")
  EcoVarPredOneSite$Year_GISSM <- c(rep(2024, length.out = 30), rep(2025, length.out = 30))
  EcoVarPredOneSite$Year_Shriver <- rep(c(2023,2024), length.out = 30)
  EcoVarPredOneSite$Sim_GISSM <- c(1:30, 1:30)
  EcoVarPredOneSite$Sim_Shriver <- rep(1:30, each = 2)
  
  # now, read in data from each netCDF
  
  for(n in seq(Outs)) {
    print(n)
    allData <-  open.nc(file.path(outputPath, Outs[n]))
   # match with attribute file
    a_idx <- grep(substr(Outs[n], 1, nchar(Outs[n])-9), paste0(attributes$Name, "_"))
    max_t <- attributes[a_idx, 'time_values_max']
    var <- attributes[a_idx, 'var_name']
    
    # get data
    siteData <- var.get.nc(allData, variable = var, start = c(longInd, latInd, 1), 
                           count = c(1, 1, max_t))
    
    #if(attributes[a_idx, 'TP'] == 'P') siteData <- c(rep(NA, 198), siteData)
    # add appropriate NAs either before or after the length of the data to slot it into the right place in the time series 
    if (length(siteData) == 549) {
      DataOneSite[,n] <- siteData
      names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
    } else if (length(siteData) == 180) {
      DataOneSite[,n] <- c(siteData, rep(NA,length.out = 549-180))
      names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
    } else if (length(siteData) == 32) {
      EcoVarHistOneSite[,Outs[n]] <- siteData
    } else if (length(siteData) == 60) {
      EcoVarPredOneSite[,Outs[n]] <- siteData
    } else if (length(siteData) == 352) {
      DataOneSite[,n] <- c(rep(NA,length.out = 549-352), siteData)
      names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
    }
    
    close.nc(allData)
  }
  # write data to the list
  siteDat[[i]] <- list("EcoDat_hist" = EcoVarHistOneSite,
                     "EcoDat_pred" = EcoVarPredOneSite, 
                     "AbioticDat_all" = DataOneSite)

  }



summary(DataOneSite)
write.csv(DataOneSite, "gridSTDF/projects/06-TestOutputs/DataOneSite.csv", row.names = FALSE)



# for(i in 1:500) {
#     siteData <- var.get.nc(allData, variable = "ta")
#     siteData2 <- allData[100, i, ]
#         if(all(!is.na(siteData2))) {
#             print(i)
#             #print(siteData2)
#         }
# }
