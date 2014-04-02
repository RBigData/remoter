pbdRscript <- function(body, cores=1, intern=FALSE, auto=TRUE)
{
  script <- tempfile()
  
  
   if (auto)
  {
    auto.header <- "suppressPackageStartupMessages(library(pbdMPI, quietly=TRUE))\ninit()\n\n"
    auto.footer <- "\n\nfinalize()"
    body <- paste(auto.header, body, auto.footer, collapse="\n")
  }
  
  conn <- file(script)
  writeLines(body, conn)
  close(conn)
  
#  ret <- system(paste("mpirun -np", cores, "Rscript", script, " &\necho $!"), intern=TRUE)
#  
#  if (intern)
#    return( ret )
#  else
#  {
#    pid <- ret[1L]
#    ret <- ret[-1L]
#    cat(paste(ret, "\n"))
#    return(ret[1L])
#  }
  ret <- system(paste("mpirun -np", cores, "Rscript", script), intern=intern)
  
  if (intern)
    return( ret )
  else
    invisible()
}

