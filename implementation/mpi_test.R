# load the package
# suppressMessages(library(pbdMPI, quietly = TRUE))
# 
# # initialize the MPI communicators
# init()
# 
# # Hello world
# message <- paste("Hello from rank", comm.rank(), "of", comm.size())
# comm.print(message, all.rank=TRUE, quiet=TRUE)
# 
# # shut down the communicators and exit
# finalize()

# --------- Example 2 -----------------------------------------------------------
# library(pbdMPI, quiet = TRUE)
# library(RNetCDF, quiet = TRUE)
# 
# ### MPI parameters
# init()
# rank <- comm.rank()
# size <- comm.size()
# 
# ### Define global dimensions and data
# nr <- 5
# nc_loc <- 4
# nc <- nc_loc * size
# data_global <- matrix(seq(1,nc*nr), nrow=nr, ncol=nc)
# data_local <- data_global[,rank*nc_loc+c(1:nc_loc)]
# 
# ### Parallel write
# filename <- "writeN_read1.nc"
# info.create()
# ncid <- create.nc(filename, format="netcdf4", mpi_comm=comm.c2f(), mpi_info=info.c2f())
# rdim <- dim.def.nc(ncid, "rows", nr)
# cdim <- dim.def.nc(ncid, "cols", nc)
# varid <- var.def.nc(ncid, "data", "NC_INT", c(rdim, cdim))
# var.put.nc(ncid, varid, data_local, start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))
# close.nc(ncid)
# info.free()
# barrier()
# 
# ### Serial read
# if (rank==0) {
#   ncid2 <- open.nc(filename)
#   data_global2 <- var.get.nc(ncid2, "data")
#   close.nc(ncid2)
# }
# 
# ### Check global data on rank 0
# if (rank==0) {
#   cat("data_global =", data_global, "\n")
#   cat("data_global2 =", data_global2, "\n")
#   if (!isTRUE(all.equal(data_global, data_global2))) {
#     comm.abort()
#   }
# }
# 
# finalize()
# 



# --------- Example 3 -----------------------------------------------------------

# ### Initial.
suppressMessages(library(pbdMPI, quietly = TRUE))
init()

### Examples.
#comm.cat(">>> block\n\", quiet = TRUE)
# jid <- get.jid(7, method = "block")
# comm.print(jid, all.rank = TRUE)

# comm.cat(\">>> cycle\n\", quiet = TRUE)
# jid <- get.jid(7, method = "cycle")
# comm.print(jid, all.rank = TRUE)
# 
# comm.cat(\">>> block (all)\n\", quiet = TRUE)
alljid <- get.jid(n = 10000:10050, method = "block", all = TRUE)
comm.print(alljid)

# comm.cat(\">>> cycle (all)\n\", quiet = TRUE)
# alljid <- get.jid(7, method = \"cycle\", all = TRUE)
# comm.print(alljid)

### Finish.
finalize()





suppressMessages(library(pbdMPI, quietly = TRUE)) 
init() 
set.seed(123) 
x <- matrix(rnorm(1000000), 1000) 
parMM.spmd <- function(x, y){ 
  id <- get.jid(nrow(x)) 
  do.call(rbind, allgather(x[id,] %*% y)) } 

time.proc <- system.time(replicate(10, parMM.spmd(x, x))) 
comm.print(time.proc) finalize()




library(pbdMPI, quiet = TRUE)
# init()
# .comm.size <- comm.size()
# .comm.rank <- comm.rank()
# 
# msg <- sprintf("Hello world from process %d\n", .comm.rank)
# comm.cat("Say hello:\n", quiet = TRUE)
# comm.cat(msg, all.rank = TRUE)
# 
# k <- 10
# x <- rep(.comm.rank, k)
# comm.cat("\nOriginal x vector:\n", quiet = TRUE)
# comm.print(x, all.rank = TRUE)
# 
# y <- allgather(x, unlist = TRUE)
# A <- matrix(y, nrow = k, byrow = FALSE)
# comm.cat("\nAllgather matrix (only showing process 0):\n", quiet = TRUE)
# comm.print(A)
# 
# finalize()

init()
n <- 100 
x <- split((1:n) + n * comm.rank(), rep(1:10, each = 10))
sm <- pbdLapply(x, sum) 
comm.print(unlist(sm)) 
finalize()
