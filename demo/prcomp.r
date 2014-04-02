library(pbdinline)

body <- '
  library(pbdDMAT, quietly=TRUE)
  init.grid()
  
  m <- 1000
  n <- 25
  x <- ddmatrix("rnorm", m, n)
  
  x
  
  prcomp(x)
  
  finalize()
'

pbdRscript(body=body, cores=2, auto=FALSE)
