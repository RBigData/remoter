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
#' @param port
#' The port to use for communication between the client and rank 0.
#' 
#' @details
#' The \code{port} values between the client and server \emph{MUST}
#' agree.  If they do not, this can cause the client to hang.
#' 
#' The servers are launched via \code{pbdRscript()}.
#' 
#' The client is a specialized REPL that intercepts commands sent
#' through the R interpreter.  These commands are then sent from the
#' client to and evaluated on the servers.
#' 
#' The client communicates over ZeroMQ with MPI rank 0 (of the 
#' servers) using a REQ/REP pattern.  Both commands (from client to
#' server) and returns (from servers to client) are handled in this
#' way.  Once a command is sent from the client to MPI rank 0,
#' that command is then "broadcasted" from MPI rank 0 to the other
#' MPI ranks.  The method of broadcast is handled by the input
#' \code{bcast_method}.  If \code{bcast_method="mpi"}, then \code{MPI_bcast}
#' is used to transmit the command.  Otherwise (\code{bcast_method="zmq"})
#' uses ZeroMQ with a PUSH/PULL pattern.  The MPI method is probably
#' epsilon faster, but it will busy-wait.  The ZeroMQ bcast method
#' will not busy wait, in addition to the other benefits ZeroMQ
#' affords; thus, \code{bcast_method="zmq"} is the default.
#' 
#' To shut down the servers and the client, use the command \code{pbd_exit()}.
#' 
#' @examples
#' \dontrun{
#' library(pbdCS)
#' pbd_launch_servers()
#' pbd_launch_client()
#' }
#' 
#' @rdname launchers
#' @seealso \code{\link{pbdRscript}, \link{pbd_exit}}
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


