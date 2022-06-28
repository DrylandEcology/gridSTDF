########## per RNetCDF
library(RNetCDF)
numRows <- 1386 # nrows
numCols <- 585 #ncols
days <- 1095 # 3 years worth of days!

filename <- "test2.nc" 

ncid <- create.nc(filename, format="netcdf4", clobber = TRUE)#, mpi_comm=comm.c2f(), mpi_info=info.c2f())
rdim <- dim.def.nc(ncid, "rows", numRows)
cdim <- dim.def.nc(ncid, "cols", numCols)
tdim <- dim.def.nc(ncid, "time", days)
varid <- var.def.nc(ncid, "VWC", "NC_FLOAT", c(rdim, cdim, tdim))

print.nc(ncid)
var.inq.nc(ncid, 0)
file.inq.nc(ncid)

var.put.nc(ncfile = ncid, 
           variable = varid, 
           data  = AnomalyData1[,'VWC.Shallow'], # the data
           start=c(1, 1, 1), 
           count=c(1, 1, days))

close.nc(ncid)
info.free()
barrier()


open.nc(filename)
data <- var.get.nc(ncfile = ncid, 
                   variable = varid)


break
# ------------------------------------------------------------------------------------
library(rSW2st)

# spin up netcdfs
# first example - xyzt - soil moisture at multiple layers for one year
exampDat <- read_netCDF(
  "Data/www.northwestknowledge.net/metdata/data/tmmx_2021.nc",
  time_name = "day"
)

# Prepare attribute lists
nc_att_global <- list(
  title = "Example netCDF for gridSTDF output",
  version = paste0("v", format(Sys.Date(), "%Y%m%d")),
  source_id = "SOILWAT2",
  further_info_url = "https://github.com/DrylandEcology/",
  source_type = "LAND",
  realm = "land",
  product = "model-output",
  grid_label = "gn",
  nominal_resolution = "7 km"
)

nc_att_crs <- list(
  crs_wkt = sf::st_crs("EPSG:4326")$Wkt,
  grid_mapping_name = "latitude_longitude",
  longitude_of_prime_meridian = 0,
  semi_major_axis = 6378137.0,
  inverse_flattening = 298.257223563
)

nc_att_xy <- list(name = c("lon", "lat"), 
                  standard_name = c("longitude","latitude"), 
                  long_name = c("longitude", "latitude"), 
                  units = c("degrees_east","degrees_north"))

n_soillayers <- 3
data_dims <- c(
  ns = 0,
  nx = length(exampDat[["xyspace"]][["x"]]),
  ny = length(exampDat[["xyspace"]][["y"]]),
  nz = n_soillayers,
  nt = length(exampDat[["time_values"]]),
  nv = 0
)

# create an empty ncdf4 with the correct lat, long parameters
create_netCDF(
  filename = 'test.nc',
  overwrite = TRUE,
  xyspace = exampDat[["xyspace"]],
  data = NULL,
  data_str = "xyzt",
  data_dims = data_dims,
  var_attributes = list(name = "soil moisture", units = "VWC"), # this would be many in the future
  xy_attributes = nc_att_xy,
  crs_attributes = nc_att_crs,
  time_values = exampDat[["time_values"]],
  type_timeaxis = "timeseries",
  vertical_values = seq_len(n_soillayers), # provide vertical attributes
  global_attributes = nc_att_global
)



