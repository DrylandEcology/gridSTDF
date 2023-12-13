module load netcdf-c/4.9.0
module load R/4.2.0
module load geos/3.8.1 gdal/3.0.4 proj/6.2.1 
R
library(stars)
library(RNetCDF)
library(ncmeta)
library(plyr)

attributes <- read.csv('gridSTDF/projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

outputPath <- "gridSTDF/projects/05-Setup-futureMonthly-netCDFs/Outputs/Test3_12262022"

Outputs <- list.files('gridSTDF/projects/05-Setup-futureMonthly-netCDFs/Outputs/Test3_12262022')
TempOuts <- grep('tmean|tmmx', Outputs, value = TRUE) # should get nine variables as it stands
VWCOuts <- grep('vwc-med',  Outputs, value = TRUE)

Outs <- Outputs

lat <- 35.1983
long <- -111.6513

# Get indices once ... will always be the same across out netCDF files -------------------------------
allData <- open.nc(file.path(outputPath, Outs[1]))
allLat <- var.get.nc(allData, "lat")
allLon <- var.get.nc(allData, "lon")
LonStartIdx <- which.min(abs(allLon - long))
LatStartIdx <- which.min(abs(allLat - lat))
allLat[244]

DataOneSite <- as.data.frame(matrix(nrow = 549, ncol = 9))
for(n in seq(Outs)) {
    print(n)
    allData <-  open.nc(file.path(outputPath, Outs[n]))

    # match with attribute file
    a_idx <- grep(substr(Outs[n], 1, nchar(Outs[n])-10), attributes$Name)
    max_t <- attributes[a_idx, 'time_values_max']
    var <- attributes[a_idx, 'var_name']

    # get data
    siteData <- var.get.nc(allData, variable = var, start = c(500, 85, 1), 
                            count = c(1, 1, max_t))

    if(attributes[a_idx, 'TP'] == 'P') siteData <- c(rep(NA, 198), siteData)
    DataOneSite[,n] <- siteData
    names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
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
