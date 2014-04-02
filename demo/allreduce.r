body <- "
  library(pbdMPI)
  init()
  
  x <- allreduce(1)
  
  comm.print(x)
  
  finalize()
"

pbdfunction(body=body, cores=2)
