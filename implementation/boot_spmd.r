# File name: boot_spmd.r
# Run: mpiexec -np 4 Rscript implementation/boot_spmd.r

rm(list = ls())                                       # Clean environment
library(pbdMPI, quiet = TRUE)                         # Load library
comm.set.seed(123, diff = TRUE)                       # set seed
if(comm.size() != 4)
  comm.stop("4 processors are required.")

### Load data
X <- as.matrix(iris[, -5])                            # Dimension 150 by 4
N <- nrow(X)
p <- ncol(X)

### Distribute job tasks
N.jobs <- 1000
jid <- get.jid(N.jobs)
comm.print(jid)

ret.boot <- NULL 
for(i in jid){

  id <- sample(1:N, N, replace = TRUE)
  #comm.print(id)

  ret.boot <- cbind(ret.boot, colMeans(X[id,]))
}

### Gather results from other job tasks
ret.boot <- allgather(ret.boot)
ret.boot <- do.call("cbind", ret.boot)

### Obtain CI for means
ret.ci <- apply(ret.boot, 1,
                quantile, probs = c(0.025, 0.975))    # 95% CI
comm.print(ret.ci)

### Finish
finalize()
