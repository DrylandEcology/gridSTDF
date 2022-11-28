 # rm(list = ls(all = TRUE))
 #library(RNetCDF)
 #source('functions/netcdf_functions.R')

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
#         out_file = 'projects/03-Make-Climatologies-netCDF/west.western_region.nc1' ,
#         overwrite = TRUE,
#         cdo_output = TRUE)

# ------------------------------------------------------------------------------
# Step 1 -----------------------------------------------------------------------
# ------------------------------------------------------------------------------
Output_folder <- 'projects/05-Setup-futureMonthly-netCDFs/Outputs/Test1_11222022/'

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

# 1) static attirbutes - same for each run

nc_att_crs <- example1$crs_attributes
nc_att_xy <- western_region.nc1$xy_attributes


# daily
c1 <- as.character(seq(as.Date("1991/1/1"), as.Date("1992/07/02"), "days"))
c2 <-  as.character(seq(as.Date("2020/1/1"), as.Date("2021/07/02"), "days"))
#c2 <- c2[-60]
time_bounds_daily = matrix(c(c1, c2), 
                     nrow = length(1:549), ncol = 2)

# monthly
c1 <- as.character(seq(as.Date("1991/1/15"), as.Date("1991/12/15"), "months"))
c2 <-  as.character(seq(as.Date("2020/1/15"), as.Date("2020/12/15"), "months"))
time_bounds_monthly = matrix(c(c1, c2), 
                           nrow = length(1:12), ncol = 2)

# annually
c1 <- as.character(seq(as.Date("1991/1/1"), as.Date("2020/1/1"), "years"))
c2 <-  as.character(seq(as.Date("1991/12/31"), as.Date("2020/12/31"), "years"))
time_bounds_annually = matrix(c(c1, c2), 
                             nrow = length(1:30), ncol = 2)

attributes <- read.csv('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

names <-  attributes$short_name

for(nc in 1:nrow(attributes)){
  
  # 2) - Change for each run -----------------------------------------------------
  # var_attributes ---------------------------------------------------------------
  nc_var1 <- list(
    name = attributes$var_name[nc],
    long_name = attributes$var_standardname[nc],
    units = attributes$var_units[nc], 
    description = attributes$var_description[nc],
    comment = attributes$var_comment[nc],
    cell_methods = attributes$var_cell_methods[nc],
    prec = "float")
  
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
  time_bounds <- if(nc_time$units == "days") {
    time_bounds_daily 
  } else if(nc_time$units == "months") {
    time_bounds_monthly
  } else if(nc_time$units == "years") {
    time_bounds_annually
  }

  ## data_dims ---------------------------------
  data_dims_nc <- c(0, 739, 585, 0, nrow(time_bounds), 0)
  names(data_dims_nc) <- c("ns", "nx", "ny", "nz", "nt", "nv")
  
  # global ----------------------------------------------------------------------
  nc_att_global <- list(
    title = "Short-term drought forecasts based on NWS long-leads for the western U.S.",
    created_date = paste0( format(Sys.Date(), "%Y-%m-%d")),
    version = paste0("v.", format(Sys.Date(), "%b%Y")),
    created_by = paste(version$version.string, '; R packages: pbdncdf4', packageVersion("pbdNCDF4")),
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

  
  assign(names[nc], create_netCDF(
    filename = file.path(Output_folder,
                         paste0(attributes$Name[nc], '_', format(Sys.Date(), "%m%Y"), '.nc')),
    overwrite = TRUE,
    xyspace = western_region.nc1[["xyspace"]],
    #data = western_region.nc1[["data"]],
    data_str = "xyt",
    data_dims = data_dims_nc,
    var_attributes = nc_vars,
    xy_attributes = nc_att_xy,
    crs_attributes = nc_att_crs,
    time_values = attributes$time_values_min[nc]:attributes$time_values_max[nc],
    time_attributes = nc_time,
    time_bounds = time_bounds, 
    type_timeaxis = "climatology",
    global_attributes = nc_att_global, 
    isParallel = FALSE
  ))
  
}

comm.print("creation done")


