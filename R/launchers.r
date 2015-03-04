#' Client/Server Launchers
#' 
#' Launchers for the pbdR client/server.
#' 
#' @description
#' This function is a simple wrapper around the system() command.  As such,
#' data is not shared between the calling R process and the batch processes
#' which execute the 'body' source.
#' 
#' @param nranks 
#' The number of MPI ranks to launch.
#' @param bcast_method
#' The method used by the servers to communicate.  Options are "zmq"
#' for ZeroMQ-based communication, or "mpi" for 
#' @param intern
#' logical; determines if the output of the evaluation of the
#' script should be saved as an R character vector.
#' @param port
#' The port to use for communication between the client and rank 0.
#' 
#' @details
#' TODO: describe structure
#' 
#' @rdname launchers
#' 
#' @export
pbd_launch_servers <- function(nranks=2, bcast_method="zmq", port=5555)
{
  bcast_method <- match.arg(tolower(bcast_method), c("zmq", "mpi"))
  
  rscript <- paste0("
    suppressPackageStartupMessages(library(pbdCS))
    pbdenv$whoami <- 'remote'
    pbdenv$port <- ", port, "
    pbdenv$bcast_method <- \"", bcast_method, "\"
    pbdCS:::pbd_repl()
    finalize()
  ")
  
  pbdRscript(rscript, nranks=nranks, auto=TRUE, pid=FALSE, wait=FALSE)
  
  invisible(TRUE)
}



#' @rdname launchers
#' 
#' @export
pbd_launch_client <- function(port=5555)
{
  pbdenv$whoami <- "local"
  pbdenv$port <- port
  
  pbd_repl()
  
  invisible(TRUE)
}


