# a simple test script to test writing netCDFS on the HPC using pbdNCDF4

library(pbdNCDF4, quietly = TRUE)
library(pbdMPI, quietly=TRUE)
# ------------------------------------------------------------------------------
# Step 1 -----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# make the output directory
#Output_folder <- paste0('./outputs/pbdCDF_test/')

#if (!file.exists(Output_folder)){
#  dir.create(Output_folder)    
#}

# initalize the paralellization of the job
pbdMPI::init()

rank <- comm.rank()
size <- comm.size()

hostname <- spmd.get.processor.name()


### define data dimension.
n <- size * 4
p <- 5

### set up slab for this process
st <- c(rank * n/ size + 1, 1)
co <- c(n / size, p)

### generate local matrix portion
x <- matrix(rank + 1, nrow = n / size, ncol = p) +
  rep(1:p, each = n / size)

### set up dimensions for full matrix (not just local slab)
rdim <- ncdim_def("rows", "number", vals = 1:n)
cdim <- ncdim_def("columns", "number", vals = 1:p)

### define matrix variable in file (must not create full storage!!)
x.nc_var <- ncvar_def("testMatrix", "count",
                      list(rows = rdim, columns = cdim),
                      missval = -1, prec = "integer")

### create (collectively) in parallel a file with given dimensions
nc <- nc_create_par("test_par.nc", x.nc_var)
nc_var_par_access(nc, x.nc_var)

### write variable values to file
ncvar_put(nc, "testMatrix", as.vector(x), start = st, count = co)
nc_sync(nc)

### close file
nc_close(nc)

# # make netCDF 
# pbdNCDF4::nc_create_par(filename = comm.cat("file_",rank,"of_",size,"onHost_",hostname, all.rank = TRUE, quiet = TRUE), 
#                     vars = ,
#                     force_v4 = TRUE)


finalize()

