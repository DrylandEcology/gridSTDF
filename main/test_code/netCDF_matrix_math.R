rank <- 1
size <- 5

numRows <- 715 # nrows, longitude
numCols <- 567 #ncols, latitude
days <- 365 # 1 years worth of days!

# split metric
split <-  floor(numRows / size)

### First set up "slab" just for an individual processor
st <- c(rank * split + 1, 1, 1)  # start argument for this processor
co <- c(split, numCols, days) # count argument for this processor (and all processors)

### generate data structure that holds data
x2 <- array(data = 1, dim = c(split, numCols, days))
# x <- matrix(rank + 1, nrow = split, ncol = numCols) +
#   rep(1:numCols, each = split)





size <- 2
### define data dimension. 
n <- size * 4
p <- 5

split <-  (n / size)

### set up slab for this process
st <- c(rank * split + 1, 1)
co <- c(split, p)

### generate local matrix portion
x <- matrix(rank + 1, nrow = n / size, ncol = p) +
  rep(1:p, each = n / size)
