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
  
  if (same.str(get.os(), "windows"))
    stop("doesn't work :[")
  else
    ret <- system(paste("mpirun -np", cores, "Rscript", script), intern=intern)
  
  if (intern)
    return( ret )
  else
    invisible()
}



killhung <- function(pid)
{
  os <- get.os()
  
  if (same.str(os, "windows"))
    stop("doesn't work :[")
  else
    system(paste("kill", pid))
  
  invisible()
}



same.str <- function(str1, str2)
{
  return( tolower(str1) == tolower(str2) )
}


get.os <- function()
{
  ret <- Sys.info()["sysname"]
  return( ret )
}
