# rm(list = ls(all = TRUE))
# source('functions/netcdf_functions.R')

file1 <- "main/Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc"
temp.nc1 <- rSW2st::read_netCDF(file1, method = "array", xy_names = c("lon", "lat"))
#temp.nc11 <- open.nc("main/Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc")

# Prepare attribute lists
nc_att_global <- list(
  title = "Maximum temperature test",
  version = paste0("v", format(Sys.Date(), "%Y%m%d")),
  source_id = "SOILWAT2",
  further_info_url = "https://github.com/DrylandEcology/",
  source_type = "LAND",
  realm = "land",
  product = "model-output",
  geospatial_bounds_crs = "EPSG:4326")

nc_att_crs <- list(
  crs_wkt = sf::st_crs("EPSG:4236")$Wkt,
  grid_mapping_name = "latitude_longitude",
  # GRS 1980 ellipsoid
  longitude_of_prime_meridian = 0,
  semi_major_axis = 6378137.0,
  inverse_flattening = 298.257222101
)

nc_att_xy <- list(
  name = c("lon", "lat"),
  standard_name = c("longitude", "latitude"),
  long_name = c("longitude", "latitude"),
  units = c("degrees_east", "degrees_north")
)

## Write netCDF for gridded data

tmmx_nc <- create_netCDF(
  filename = "test_maxtemp.nc",
  overwrite = TRUE,
  xyspace = temp.nc1[["xyspace"]],
  data = temp.nc1[["data"]],
  data_str = "xyt",
  var_attributes = list(name = "tmmx", units = "C", description = "Daily Maximum Temperature (2m)", prec = "float"),
  xy_attributes = nc_att_xy,
  crs_attributes = nc_att_crs,
  time_values = 1:365,
  time_bounds = matrix(rep(1:365, 2), nrow = length(1:365), ncol = 2),
  type_timeaxis = "timeseries",
  global_attributes = nc_att_global, 
  isParallel = FALSE
)

# temp.nc2 <- read_netCDF("test_maxtemp.nc", method = "array", xy_names = c("lon", "lat"))
# temp.nc22 <-  open.nc("test_maxtemp.nc")
# print.nc(temp.nc11)
# print.nc(temp.nc22)
# print(temp.nc1)
# #summary((temp.nc1$data))
# print(temp.nc2)
# #summary((temp.nc2$data))





