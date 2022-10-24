rm(list = ls(all = TRUE))
library(RNetCDF)
source('functions/netcdf_functions.R')

#devtools::install_github("r4ecology/rcdo", dependencies = TRUE, force = TRUE)
# Clip file to our domain if you haven't already ----------------------------
#library(rcdo)
# source('projects/03-Make-Climatologies-netCDF/nc_clip_edit.R')
# 
# file1 <- "main/Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc"
# 
# # Clip and write file1
# nc_clip(file1,
#         lon_range = c(-125, -94.0 ),
#         lat_range = c(25, 49.50000),
#         out_file = 'projects/03-Make-Climatologies-netCDF/west.temp.nc1' ,
#         overwrite = TRUE,
#         cdo_output = TRUE)

# ------------------------------------------------------------------------------
# Step 1 -----------------------------------------------------------------------
# ------------------------------------------------------------------------------

example1 <- '~/Downloads/Air_temperature/ta_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc'
example1 <- '~/Downloads/Precipitation_amount/pr_yr_SOILWAT2_RangeDroughtExposure_historical_gn_19710101-20101231-clim.nc'

example1 <-  rSW2st::read_netCDF(example1, method = "array", 
                                 xy_names = c("lon", "lat"), time_name = "day")
example1$var_attributes
example1$time_attributes
example1$time_bounds
example1$values
example1$crs_attributes
example1$crs
example1$global_attributes

file1 <- "projects/03-Make-Climatologies-netCDF/west.temp.nc1"
#file1 <- "main/Data/www.northwestknowledge.net/metdata/data/pr_2022.nc"
#temp.nc1 <- open.nc(file1)
temp.nc1 <- rSW2st::read_netCDF(file1, method = "array", 
                                xy_names = c("lon", "lat"), 
                                time_name = "day")

temp.nc1$var_attributes
temp.nc1$time_attributes
temp.nc1$crs_attributes
temp.nc1$crs
temp.nc1$global_attributes


# ------------------------------------------------------------------------------
# Step 2 - Prepare attributes --------------------------------------------------=
# ------------------------------------------------------------------------------

# 1) static attirbutes - same for each run

nc_att_crs <- example1$crs_attributes
nc_att_xy <- temp.nc1$xy_attributes

c1 <- as.character(seq(as.Date("1991/1/1"), as.Date("1991/12/31"), "days"))
c2 <-  as.character(seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"))
c2 <- c2[-60]
time_bounds = matrix(c(c1, c2), 
                     nrow = length(1:365), ncol = 2)

attributes <- read.csv('projects/03-Make-Climatologies-netCDF/nc_atts.csv')

for(nc in 1:3){
  
  # 2) - Change for each run -----------------------------------------------------
  # var_attributes ---------------------------------------------------------------
  nc_var1 <- list(
    name = attributes$var_standardname[nc],
    long_name = attributes$var_longname[nc],
    units = attributes$var_units[nc], 
    description = attributes$var_description[nc],
    comment = attributes$var_comment[nc],
    methods = attributes$var_methods[nc],
    prec = "float")
  
  nc_vars <- nc_var1
  
  # time_attributes --------------------------------------------------------------
  nc_time <- list(
    units = "days",
    calendar = "standard",
    long_name = "time",
    axis = "T",
    unlim = FALSE
  )
  
  # global ----------------------------------------------------------------------
  nc_att_global <- list(
    title = "Short-term drought forecasts based on NWS long-leads for the western U.S.",
    created_date = paste0( format(Sys.Date(), "%Y-%m-%d")),
    version = paste0("v.", format(Sys.Date(), "%b%Y")),
    created_by = paste(version$version.string, '; R packages: pbdncdf4', packageVersion("pbdncdf4")),
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
  
  
  
  
  
  ## Write netCDF for gridded data
  
  tmmx_nc <- create_netCDF(
    filename = file.path('projects/03-Make-Climatologies-netCDF/Outputs/',
                         paste0(attributes$Name[nc], '_', format(Sys.Date(), "%m%Y"), '.nc')),
    overwrite = TRUE,
    xyspace = temp.nc1[["xyspace"]],
    data = temp.nc1[["data"]],
    data_str = "xyt",
    var_attributes = nc_vars,
    xy_attributes = nc_att_xy,
    crs_attributes = nc_att_crs,
    time_values = 1:365,
    time_attributes = nc_time,
    time_bounds = time_bounds, 
    type_timeaxis = "climatology",
    global_attributes = nc_att_global, 
    isParallel = TRUE
  )
  
}


# check <-  rSW2st::read_netCDF('projects/03-Make-Climatologies-netCDF/Outputs/tmmx_gridSTDF_historical_19910101-20201231-clim-102022.nc',
#                               method = "array", 
#                                          xy_names = c("lon", "lat"), 
#                                          time_name = "day")
# 
# check <- pbdNCDF4::nc_open('projects/03-Make-Climatologies-netCDF/Outputs/tmmx_gridSTDF_historical_19910101-20201231_clim_102022.nc')
# check
