### SHELL> mpiexec -np 4 Rscript --vanilla main/implementation/pbdMPI-hello.R 

library(pbdMPI, quietly=TRUE)

init()

id <- comm.rank()
np <- comm.size()

hostname <- spmd.get.processor.name()

comm.cat("Hello world from process", id, "of", np, "on host", 
         hostname, "\n", all.rank=TRUE, quiet=TRUE)

finalize()