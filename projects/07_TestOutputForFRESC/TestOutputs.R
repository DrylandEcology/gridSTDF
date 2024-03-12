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
library(sf)

attributes <- read.csv('./main/implementation/nc_atts-all.csv')

  outputPath <- "./outputs/20240311/"

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

# determine which grid-cells overlay this polygon --------------------------
Sites <- as.data.frame(data.table::fread("main/Data/WeatherDBSitesTable_WestIndex.csv"))
Sites_sfc <- st_sfc(lapply(1:nrow(Sites), FUN = function(x) {
  #st_point(c(x[2], x[1]))
  st_point(c(Sites[x,"Longitude"], Sites[x,"Latitude"]))
}))
Sites_sf <- st_sf(Sites, geometry = Sites_sfc, crs = "EPSG:4326")

# read in bounding polygon
poly <- st_read("./projects/07_TestOutputForFRESC/FRESC_testBox/", "GriddedDroughtSubset")
# Find the centroids that overlap with the bounding polygon 
whichSites <- Sites_sf[st_intersects(poly, Sites_sf)[[1]],]

# read in data about "Sites" (grid cells)
Sites <- st_drop_geometry(whichSites) 

# make a list to hold data for each site 
siteDat <- vector(mode = "list", length = 2)# nrow(Sites))

# get data for each grid cell in turn 
for (i in 1:nrow(Sites)) { 
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
    # get name for label of column

    # get data
    if (a_idx %in% c(59, 60, 58)) {
      siteData <- var.get.nc(allData, variable = "ta", start = c(longInd, latInd, 1), 
                             count = c(1, 1, max_t))
    } else {
      siteData <- var.get.nc(allData, variable = var, start = c(longInd, latInd, 1), 
                             count = c(1, 1, max_t))
      
    }
    
    #if(attributes[a_idx, 'TP'] == 'P') siteData <- c(rep(NA, 198), siteData)
    # add appropriate NAs either before or after the length of the data to slot it into the right place in the time series 
    if (length(siteData) == 549) {
      DataOneSite[,n] <- siteData
      names(DataOneSite)[n] <- substr(Outs[n], 1, nchar(Outs[n])-10)
      #names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
    } else if (length(siteData) == 180) {
      DataOneSite[,n] <- c(siteData, rep(NA,length.out = 549-180))
      names(DataOneSite)[n] <- substr(Outs[n], 1, nchar(Outs[n])-10)
      #names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
    } else if (length(siteData) == 32) {
      EcoVarHistOneSite[,Outs[n]] <- siteData
    } else if (length(siteData) == 60) {
      EcoVarPredOneSite[,Outs[n]] <- siteData
    } else if (length(siteData) == 352) {
      DataOneSite[,n] <- c(rep(NA,length.out = 549-352), siteData)
      names(DataOneSite)[n] <- substr(Outs[n], 1, nchar(Outs[n])-10)
      #names(DataOneSite)[n] <- attributes[a_idx, 'dataset_column_name']
    }
    
    # get date information
    if (n == 7) {
      timeVar <- var.get.nc(allData, "time")
    }
    
    close.nc(allData)
  }
  # add a variable for Date
  
  DataOneSite$V1 <- timeVar
  names(DataOneSite)[1] <- "DaysSince1970"
  
  # write data to the list
  siteDat[[i]] <- list("EcoDat_hist" = EcoVarHistOneSite,
                       "EcoDat_pred" = EcoVarPredOneSite, 
                       "AbioticDat_all" = DataOneSite)
  
}
# 
# 
# 
# summary(DataOneSite)
# write.csv(DataOneSite, "gridSTDF/projects/06-TestOutputs/DataOneSite.csv", row.names = FALSE)


# Plot data for one site --------------------------------------------------
oneSite <- siteDat[[1]]
oneSite_met <- oneSite$AbioticDat_all
# convert "DaysSince1970" to date
oneSite_met$V2 <- as.Date(oneSite_met$DaysSince1970, origin = '1970-01-01')
names(oneSite_met)[2] <- "Date"

# for precip
ggplot(oneSite_met) + 
  #geom_line(aes(x = Date, y = ppt_dy_gridSTDF_recentpast)) + 
  geom_line(aes(x = Date, y = `ppt_dy_gridSTDF_recentpast-rollsum`)) + 
  geom_line(aes(x = Date, y = oneSite_met$'ppt_dy_gridSTDF_median-prediction')) + 
  geom_line(aes(x = Date, y = oneSite_met$'ppt_dy_gridSTDF_10pct-prediction'), col = 'blue', lty = 2) + 
  geom_line(aes(x = Date, y = oneSite_met$'ppt_dy_gridSTDF_90pct-prediction'), col = 'blue', lty = 2)

# for temp
ggplot(oneSite_met) + 
  #geom_line(aes(x = Date, y = tmean_dy_gridSTDF_recentpast)) + 
  geom_line(aes(x = Date, y = `tmean_dy_gridSTDF_recentpast`)) + 
  geom_line(aes(x = Date, y = oneSite_met$'tmean_dy_gridSTDF_median-prediction')) + 
  geom_line(aes(x = Date, y = oneSite_met$'tmean_dy_gridSTDF_10pct-prediction'), col = 'blue', lty = 2) + 
  geom_line(aes(x = Date, y = oneSite_met$'tmean_dy_gridSTDF_90pct-prediction'), col = 'blue', lty = 2)


# for soil moisture
ggplot(oneSite_met) + 
  #geom_line(aes(x = Date, y = ppt_dy_gridSTDF_recentpast)) + 
  geom_line(aes(x = Date, y = `vwc-deep_dy_gridSTDF_historical_19910101-20201231-median`), col = "darkgreen") + 
  geom_line(aes(x = Date, y = `vwc-deep_dy_gridSTDF_historical_19910101-20201231-10pct`), col = "green") + 
  geom_line(aes(x = Date, y = `vwc-deep_dy_gridSTDF_historical_19910101-20201231-90pct`), col = "green") + 
  geom_line(aes(x = Date, y = `vwc-deep_dy_gridSTDF_recentpast-rollmean`), lty = 3) + 
  geom_line(aes(x = Date, y = `vwc-deep_dy_gridSTDF_median-prediction`), lty = 3) + 
  geom_line(aes(x = Date, y =`vwc-deep_dy_gridSTDF_10pct-prediction`), col = 'blue', lty = 2) + 
  geom_line(aes(x = Date, y = `vwc-deep_dy_gridSTDF_90pct-prediction`), col = 'blue', lty = 2)


# for(i in 1:500) {
#     siteData <- var.get.nc(allData, variable = "ta")
#     siteData2 <- allData[100, i, ]
#         if(all(!is.na(siteData2))) {
#             print(i)
#             #print(siteData2)
#         }
# }


# get gridded data for all sites ------------------------------------------

pptPred_all <- stars::read_ncdf("./projects/07_TestOutputForFRESC/trimmed_netCDFs/ppt_dy_gridSTDF_median-prediction_032024.nc.nc", 
                                  var = "ppt", proxy = FALSE) 
st_crs(pptPred_all) <- 4326

pptPred_trim <- st_transform(pptPred_all, st_crs(poly)) # datum transformation

# trim to one date, just to make things simpler
pptPred_oneDate <- pptPred_trim["ppt", , , 300]
mapview(pptPred_oneDate)


# trim to one date, just to make things simpler
tMeanHist_oneDate <-  tmeanHist_all["ta", , , 10]

test <- st_crop(tmeanHist_trim, poly, crop = TRUE)

plot(tMeanHist_oneDate, border = NA, reset = FALSE)

#Crop to bounding box (insert coordinates in ...)
bb <- sf::st_bbox(poly)
stars_object <- tmeanHist_trim[bb]

#plot
ggplot()+
  geom_stars(data = stars_object) +
  geom_sf(data = sf_object)





colrow_from_xy = function(x, obj, NA_outside = FALSE) {
  if (inherits(obj, "stars"))
    obj = st_dimensions(obj)
  xy = attr(obj, "raster")$dimensions
  if (inherits(obj, "dimensions"))
    gt = st_geotransform(obj)
  
  if (isTRUE(st_is_longlat(st_crs(obj)))) {
    bb = st_bbox(obj)
    # see https://github.com/r-spatial/stars/issues/519 where this is problematic;
    # not sure whether this introduces new problems.
    #		sign = ifelse(x[,1] < bb["xmin"], 1., ifelse(x[,1] > bb["xmax"], -1., 0.))
    #		x[,1] = x[,1] + sign * 360.
    # one more try: https://github.com/r-spatial/stars/issues/563
    ix = x[,1] > bb["xmax"] & !is.na(x[,1])
    x[ix,1] = x[ix,1] - 360.
    ix = x[,1] < bb["xmin"] & !is.na(x[,1])
    x[ix,1] = x[ix,1] + 360.
  }
  if (!any(is.na(gt))) { # have geotransform
    inv_gt = gdal_inv_geotransform(gt)
    if (any(is.na(inv_gt)))
      stop("geotransform not invertible")
    ret = floor(xy_from_colrow(x, inv_gt) + 1.) # will return floating point col/row numbers!!
    if (NA_outside)
      ret[ ret[,1] < 1 | ret[,1] > obj[[ xy[1] ]]$to | ret[,2] < 1 | ret[,2] > obj[[ xy[2] ]]$to, ] = NA
    ret
  } else if (is_rectilinear(obj)) {
    ix = obj[[ xy[1] ]]$values 
    if (!inherits(ix, "intervals"))
      ix = as_intervals(ix, add_last = length(ix) == dim(obj)[ xy[1] ])
    cols = find_interval(x[,1], ix)
    iy = obj[[ xy[2] ]]$values 
    if (!inherits(iy, "intervals"))
      iy = as_intervals(iy, add_last = length(iy) == dim(obj)[ xy[2] ])
    rows = find_interval(x[,2], iy) # always NA_outside
    cbind(cols, rows)
  } else if (is_curvilinear(obj)) {
    stop("colrow_from_xy not supported for curvilinear objects")
  } else
    stop("colrow_from_xy not supported for this object")
}
  