library(pbdinline)


body <- "
  x <- ddmatrix(1:100, 10)
  
  x
  comm.print(x@Data)
"

pbdRscript(body, cores=2, auto.dmat=TRUE)
