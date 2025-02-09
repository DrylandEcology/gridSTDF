## Not run:
# This example assumes that the NetCDF library was built with MPI support,
# and that both RNetCDF and pbdMPI are installed in R.
# If the example code is stored in a file myexample.R,
# run R under MPI using a command similar to:
# SHELL> srun -A swbsc --reservation=dev -N2 -n2 -c2 Rscript RNetCDF_test.R

.libPaths("/home/astears/R/x86_64-redhat-linux-gnu-library/4.2")

library(pbdMPI)
library(RNetCDF)
suppressMessages(library(rSOILWAT2, quiet = TRUE))

suppressMessages(library(sf, quiet = TRUE))
suppressMessages(library(rSW2data, quiet = TRUE))
suppressMessages(library(RSQLite, quietly = TRUE))
suppressMessages(library(DBI, quietly = TRUE))
suppressMessages(library(rSW2st, quietly = TRUE))
suppressMessages(library(rSW2funs, quietly = TRUE))
#suppressMessages(library(raster, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))

suppressMessages(library(pbdMPI, quiet = TRUE))

#suppressMessages(library(pbdNCDF4, quiet = TRUE))
suppressMessages(library(RNetCDF, quiet = TRUE))
#suppressMessages(library(ncdf4, quiet = TRUE))


# Get MPI parameters
init()
rank <- comm.rank()
size <- comm.size()
# Define dimensions and data
nr <- 5
nc_local <- 4
nc <- nc_local * size
data_local <- matrix(rank, nrow=nr, ncol=nc_local)
# Open file for parallel access and define metadata
filename <- "myexample.nc"
info.create()
ncid <- create.nc(filename, format="netcdf4", mpi_comm=comm.c2f(), mpi_info=info.c2f())
rdim <- dim.def.nc(ncid, "rows", nr)
cdim <- dim.def.nc(ncid, "cols", nc)
varid <- var.def.nc(ncid, "data", "NC_INT", c(rdim, cdim))
# Use collective I/O
var.par.nc(ncid, "data", "NC_COLLECTIVE")
# Write data
var.put.nc(ncid, varid, data_local, start=c(1,rank*nc_local+1), count=c(nr,nc_local))
# Finish up
close.nc(ncid)
info.free()
finalize()
## End(Not run)
