library(pbdinline)


body <- '
  comm.cat("\n\nThis job is stuck; kill it with Ctrl+c\n", quiet=TRUE)
  
  comm.print(allreduce(1), quiet=TRUE)
'

pbdRscript(body=body, cores=2)

