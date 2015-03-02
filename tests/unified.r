library(pbdCS)
suppressMessages(source("../R/repl.r"))


start_pbd_servers <- function(ranks=2, bcast_method="zmq", port=5555)
{
  ### FIXME replace first 3 lines with library(pbdCS) eventually
  rscript <- paste0("
    suppressPackageStartupMessages(library(pbdCS))
    source('../R/repl.r')
    pbdenv$whoami <- 'remote'
    pbdenv$port <- ", port, "
    pbdenv$bcast_method <- \"", bcast_method, "\"
    pbd_repl()
    finalize()
  ")
  
  pbdRscript(rscript, cores=ranks, auto=TRUE, pid=FALSE, wait=FALSE)
  
  invisible(TRUE)
}

start_pbd_client <- function(bcast_method="zmq", port=5555)
{
  pbdenv$whoami <- "local"
  pbdenv$port <- port
  pbdenv$bcast_method <- bcast_method
  
  pbd_repl()
  
  invisible(TRUE)
}


start_pbd_servers()
#debug(pbd_eval)
start_pbd_client()

