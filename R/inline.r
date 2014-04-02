pbdfunction <- function(body, cores=1, intern=FALSE)
{
  script <- tempfile()
  
  conn <- file(script)
  writeLines(body, conn)
  close(conn)
  
  system(paste("mpirun -np", cores, "Rscript", script), intern=intern)
  
  invisible()
}

