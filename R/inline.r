pbdRscript <- function(body, cores=1, intern=FALSE, auto=TRUE, auto.dmat=FALSE)
{
  ### Input checks
  if (!is.character(body))
    stop("argument 'body' must be a character string")
  else if (length(body) == 0)
    stop("argument 'body' must be a non-empty character string")
  else if (length(body) > 1)
  {
    warn("function body has length > 1; only the first element will be used")
    body <- body[1L]
  }
  
  if (!is.int(cores))
    stop("argument 'cores' must be an integer")
  if (!is.logical(intern))
    stop("argument 'intern' must be logical")
  if (!is.logical(auto))
    stop("argument 'auto' must be logical")
  if (!is.logical(auto.dmat))
    stop("argument 'auto.dmat' must be logical")
    
  
  ### Dump body to temp file, execute
  script <- tempfile()
  
  if (auto.dmat)
    auto <- TRUE
  
   if (auto)
  {
    auto.header <- "suppressPackageStartupMessages(library(pbdMPI, quietly=TRUE))\ninit()\n\n"
    
    if (auto.dmat)
      auto.header <- paste(auto.header, "\n", "suppressPackageStartupMessages(library(pbdDMAT, quietly=TRUE))\ninit.grid()\n\n")
    
    auto.footer <- "\n\nfinalize()"
    body <- paste(auto.header, body, auto.footer, collapse="\n")
  }
  
  conn <- file(script)
  writeLines(body, conn)
  close(conn)
  
  
  ### manage return
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


