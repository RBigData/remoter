library(pbdinline)


body <- "
  x <- allreduce(1)
  comm.print(x)
"

pbdRscript(body=body, cores=2)

pbdRscript(body=body, cores=2, intern=TRUE)

