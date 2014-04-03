library(pbdinline)


body <- "
  x <- allreduce(1)
  comm.print(x)
"

pid <- pbdRscript(body=body, cores=2)
pid

