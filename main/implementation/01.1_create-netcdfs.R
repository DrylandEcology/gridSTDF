# Notes for me
# Currently I took out leap years... is that the right strategy ...
# Using days since 1970-01-01 for the time_Values. What about for the time bounds?


# rm(list = ls(all = TRUE))
 #library(RNetCDF) # this package has parallel capabilities now, so replace pdbNCDF4??
 library(ncdf4)
 #library(pbdNCDF4)
 #source('functions/netcdf_functions2.R')
 #source('functions/netcdf_functions_HPC.R')
# source functions
 # Begin ------------------------------------------------------------------------
 #file_list <- list.files(path = "./functions/", full.names = TRUE)
 
  # Iterate over the file list and source each file. TO DO: package all these functions
 # for (file in file_list) {
 #   print(file)
 #   source(file)
 # }
 
#devtools::install_github("r4ecology/rcdo", dependencies = TRUE, force = TRUE)
#library(rcdo)
# source('projects/03-Make-Climatologies-netCDF/nc_clip_edit.R')
# 
# file1 <- "main/Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc"
# 
# # Clip and write file1
# nc_clip(file1,
#         lon_range = c(-125, -94.0 ),
#         lat_range = c(25, 49.50000),
#         out_file = 'projects/03-Make-Climatologies-netCDF/west.western_region.nc1' ,
#         overwrite = TRUE,
#         cdo_output = TRUE)
 
 
#time_value -> days since XYZ (units)
# climatology bounds not time bounds for those historical ones
 # try and trick here "plotting date"

 # For february 29th take average of two surrounding days
 
# ------------------------------------------------------------------------------
# Step 1 -----------------------------------------------------------------------
# ------------------------------------------------------------------------------
Output_folder <- paste0('./outputs/', format(currDate, "%Y%m%d"))

if (!file.exists(Output_folder)){
    dir.create(Output_folder)    
}

example1 <- 'projects/03-Make-Climatologies-netCDF/ta_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc'

example1 <-  rSW2st::read_netCDF(example1, method = "array", 
                                 xy_names = c("lon", "lat"), time_name = "day")
# example1$var_attributes
# example1$time_attributes
# example1$time_bounds
# example1$values
# example1$crs_attributes
# example1$crs
# example1$global_attributes

file1 <- "projects/03-Make-Climatologies-netCDF/west.temp.nc1"
#file1 <- "main/Data/www.northwestknowledge.net/metdata/data/pr_2022.nc"
#western_region.nc1 <- open.nc(file1)
western_region.nc1 <- rSW2st::read_netCDF(file1, method = "array", 
                                xy_names = c("lon", "lat"), 
                                time_name = "day")

# western_region.nc1$var_attributes
# western_region.nc1$time_attributes
# western_region.nc1$crs_attributes
# western_region.nc1$crs
# western_region.nc1$global_attributes


# ------------------------------------------------------------------------------
# Step 2 - Prepare attributes --------------------------------------------------=
# ------------------------------------------------------------------------------

# 1) static attributes - same for each run

nc_att_crs <- example1$crs_attributes
nc_att_xy <- western_region.nc1$xy_attributes


# 2) determine time information ------------------------------------------------

# Create a vector of dates from 180 days before today to 365 days in the future - This is the climatology of the record we are accounting for
start <- currDate - 183
end <- currDate +  365
is_leap <- is_leap()

time_values_daily_h <- seq(start, end, by = "days")

# Generate vector of dates with the same month and day but different years (beginning and end of climatology)
today_julian <- as.numeric(currDate - ymd(paste(year(currDate), '01', '01', sep = "-"))) + 1

lastYear <- year(currDate) - 1
currYear <- year(currDate)
nextYear <- year(currDate) + 1

c1 <- c2 <- time_values_daily_h

if(today_julian <= 183) {
  
  year(c1[year(c1) == lastYear]) <- 1991
  year(c1[year(c1) == currYear]) <- 1992
  year(c1[year(c1) == nextYear]) <- 1993

  year(c2[year(c2) == lastYear]) <- 2023
  year(c2[year(c2) == currYear]) <- 2024
  year(c2[year(c2) == nextYear]) <- 2025

} else {

  year(c1[year(c1) == currYear]) <- 1991
  year(c1[year(c1) == nextYear]) <- 1992
  
  year(c2[year(c2) == currYear]) <- 2023
  year(c2[year(c2) == nextYear]) <- 2024
  
}

# if there is a leap year ... 
## When creating the climatology bounds with "real dates" we expect at least one NA (because, for example, '1991-02-29' does not exist)
## Since we are using the date format of  days since 1970-01-01 (instead of real dates), remove the NA and add another day to the end

if(any(is.na(c1))) {

  c1 <- c1[!is.na(c1)] # remove NA
  c1[(length(c1) + 1)] <- c1[length(c1)] + 1 # add one more date to the end
  
}

if(any(is.na(c2))) {
  
  c2 <- c2[!is.na(c2)] # remove NA
  c2[(length(c2) + 1)] <- c2[length(c2)] + 1 # add one more date to the end
  
}

time_bounds_daily_h <- matrix(c(as.integer(c1), as.integer(c2)), 
                               nrow = length(1:549), ncol = 2)


time_values_daily_h <- as.integer(time_values_daily_h)


# daily predicted (352!) -------------------------------------------------------
predicted_daily_start <- currDate + 14
predicted_daily_end <- currDate + 365

c1 <- as.integer(seq(predicted_daily_start, predicted_daily_end, "days"))
#c1 <- c1[!c1 %in% ("2024-02-29")]
c2 <- c1

time_bounds_daily_p <- matrix(c(c1, c2), 
                             nrow = length(1:352), ncol = 2)

time_values_daily_p <- c1

# recent past ------------------------------------------------------------------
recentpast_daily_start <- currDate - 180
recentpast_daily_end <- currDate - 1

c1 <- as.integer(seq(recentpast_daily_start, recentpast_daily_end, "days"))
#c1 <- c1[!c1 %in% ("2024-02-29")]
c2 <- c1
time_bounds_daily_rp = matrix(c(c1, c2), 
                             nrow = length(1:180), ncol = 2)

time_values_daily_rp <- c1

# historical annually (for some ecological indicators) --------------------------
c1 <- seq(as.Date(paste0(lastYear-31,"/1/1")), as.Date(paste0(lastYear,"/1/1")), "years")
c2 <- seq(as.Date(paste0(lastYear-31,"/1/1")), as.Date(paste0(lastYear,"/12/31")), "years")

time_bounds_annually_h = matrix(c(c1, c2), 
                             nrow = length(1:length(c2)), ncol = 2)

time_values_annually_h <- as.numeric(year(c1))

# annual predictions for ecological indicators ---------------------------------
c1 <- c(rep(as.Date(paste0(currYear,"/1/1")), nRuns), rep(as.Date(paste0(currYear + 1,"/1/1")), nRuns))
c2 <- c(rep(as.Date(paste0(currYear,"/12/1")), nRuns), rep(as.Date(paste0(currYear + 1,"/12/1")), nRuns))

time_bounds_annually_p = matrix(c(c1, c2), 
                                nrow = length(1:length(c2)), ncol = 2)

time_values_annually_p <- as.numeric(year(c1))

# -----------------------------------------------------------------------------
# 3) read in other information ------------------------------------------------
# -----------------------------------------------------------------------------

attributes <- read.csv('./main/implementation/nc_atts-all.csv')
names <-  attributes$short_name

for(nc in 1:100){
  #comm.print(nc)
  print(nc)
  
  # 2) - Change for each run -----------------------------------------------------
  # var_attributes ---------------------------------------------------------------
  nc_var1 <- list(
    name = attributes$var_name[nc],
    long_name = attributes$var_standardname[nc],
    units = attributes$var_units[nc], 
    description = attributes$var_description[nc],
    comment = attributes$var_comment[nc],
    cell_methods = attributes$var_cell_methods[nc],
    prec = "NC_FLOAT")
  
  nc_vars <- nc_var1
  
  # time_attributes --------------------------------------------------------------
  nc_time <- list(
    units = attributes$time_units[nc],
    calendar = "standard",
    long_name = "time",
    axis = "T",
    unlim = FALSE
  )
  
  ## time bounds --------------------------------
  time_bounds <- if(nc_time$units == "days since 1970-01-01" && attributes$TP[nc] == 'P') {
    time_bounds_daily_p
  } else if(nc_time$units == "days since 1970-01-01" && attributes$TP[nc] == 'H') {
    time_bounds_daily_h
  } else if(nc_time$units == "days since 1970-01-01" && attributes$TP[nc] == 'RP') {
    time_bounds_daily_rp
  } else if(nc_time$units == "year"  && attributes$TP[nc] == 'EH') {
    time_bounds_annually_h
  } else if(nc_time$units == "year"  && attributes$TP[nc] == 'EP') {
    time_bounds_annually_p
  }
  
  ## time values --------------------------------
  time_values <- if(nc_time$units == "days since 1970-01-01" && attributes$TP[nc] == 'P') {
    time_values_daily_p
  } else if(nc_time$units == "days since 1970-01-01" && attributes$TP[nc] == 'H') {
    time_values_daily_h
  } else if(nc_time$units == "days since 1970-01-01" && attributes$TP[nc] == 'RP') {
    time_values_daily_rp
  } else if(nc_time$units == "year"  && attributes$TP[nc] == 'EH') {
    time_values_annually_h
  } else if(nc_time$units == "year"  && attributes$TP[nc] == 'EP') {
    time_values_annually_p
  }

  ## data_dims ---------------------------------
  data_dims_nc <- c(0, 739, 585, 0, nrow(time_bounds), 0)
  names(data_dims_nc) <- c("ns", "nx", "ny", "nz", "nt", "nv")
  
  # global ----------------------------------------------------------------------
  nc_att_global <- list(
    title = "Short-term drought forecasts based on NWS long-leads for the western U.S.",
    created_date = paste0( format(currDate, "%Y-%m-%d")),
    version = paste0("v.", format(currDate, "%b%Y")),
    created_by = paste(version$version.string, '; R packages: pbdncdf4', packageVersion("ncdf4")),
    source = paste("SOILWAT2 (v6.6.0);  rSOILWAT2", "; gridSTDF"),
    further_info_url = "https://github.com/DrylandEcology/",
    institution = "Southwest Biological Science Center, U.S. Geological Survey",
    source_id = "SOILWAT2",
    source_type = "LAND",
    realm = "land",
    product = "model-output",
    geospatial_bounds_crs = "EPSG:4326",
    study_area = "Western CONUS",
    nominal_resolution = "4km",
    Conventions = "CF-1.8")
  
  
  assign(names[nc], create_netCDF(
    filename = file.path(Output_folder,
                         paste0(attributes$Name[nc], '_', format(currDate, "%m%Y"), '.nc')),
    overwrite = TRUE,
    xyspace = western_region.nc1[["xyspace"]],
    data = western_region.nc1[["data"]],
    data_str = "xyt",
    data_dims = data_dims_nc,
    var_attributes = nc_vars,
    xy_attributes = nc_att_xy,
    crs_attributes = nc_att_crs,
    time_values = time_values,
    time_attributes = nc_time,
    time_bounds = time_bounds, 
    type_timeaxis = attributes$type_timeaxis[nc],
    global_attributes = nc_att_global, 
    isParallel = isParallel # set in main runner file
  ))
  
}

if(!interactive() & isParallel) comm.print("creation done")

