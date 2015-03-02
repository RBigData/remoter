library(rzmq)
library(parallel)
source("../R/repl.r")


start_pbd_servers <- function(ranks=2, bcast_method="zmq", port=5555)
{
  ### FIXME replace first 3 lines with library(pbdCS) eventually
  rscript <- paste0("
    library(rzmq)
    library(parallel)
    source('../R/repl.r')
    pbdenv$whoami <- 'remote'
    pbdenv$port <- ", port, "
    pbdenv$bcast_method <- \\\"", bcast_method, "\\\"
    pbd_repl()
    finalize()
  ")
  
  rscript <- gsub(rscript, pattern="\\n", replacement="; ")
  rscript <- gsub(rscript, pattern=" +", replacement="")
  rscript <- sub(rscript, pattern="^;", replacement="")
  rscript <- gsub(rscript, pattern="$", replacement="\\$", fixed=TRUE)
  
  expr <- paste("mpirun -np", ranks, "Rscript -e \"", rscript, "\"")
  mcparallel(system(expr, wait=FALSE), detached=TRUE)
  
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

