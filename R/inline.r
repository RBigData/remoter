pbdRscript <- function(body, cores=1, auto=TRUE, auto.dmat=FALSE, pid=TRUE, wait=TRUE)
{
  ### Input checks
  if (same.str(get.os(), "windows"))
    stop("doesn't work :[")
  
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
#  if (!is.logical(intern))
#    stop("argument 'intern' must be logical")
  if (!is.logical(auto))
    stop("argument 'auto' must be logical")
  if (!is.logical(auto.dmat))
    stop("argument 'auto.dmat' must be logical")
    
  
  ### Dump body to temp file, execute
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
  
  script <- tempfile()
  conn <- file(script, open="wt")
  writeLines(paste0(".__the_pbd_script <- \"", script, "\""), conn)
  writeLines(body, conn)
  writeLines("unlink(.__the_pbd_script)", conn)
  close(conn)
  
  if (pbdenv$debug)
    cat("server tmpfile:  ", script, "\n")
  
  cmd <- paste("mpirun -np", cores, "Rscript", script)
  if (pid)
    cmd <- paste(cmd, "&\necho \"PID=$!\n")
  
  ret <- system(cmd, intern=FALSE, wait=wait)
  
  ### manage return
#  ret <- mcparallel(system(paste("mpirun -np", cores, "Rscript", script), intern=intern))
  
  invisible()
}

