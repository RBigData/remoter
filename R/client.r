#' Client Launcher
#' 
#' Launchers for the remoter client/server.
#' 
#' @param remote_addr
#' The remote host/address/endpoint.
#' @param port
#' The port (number) that will be used for communication between 
#' the client and server.  The port value for the client and server
#' must agree.
#' @param prompt
#' The prompt to use to delineate the client from the normal R REPL.
#' 
#' @details
#' The \code{port} values between the client and server \emph{MUST}
#' agree.  If they do not, this can cause the client to hang.
#' 
#' The client is a specialized REPL that intercepts commands sent
#' through the R interpreter.  These commands are then sent from the
#' client to and evaluated on the server.
#' The client communicates over ZeroMQ with the server using a REQ/REP pattern.
#' Both commands (from client to server) and returns (from server
#' to client) are handled in this way.
#' 
#' To shut down the server and the client, use the command \code{q()}.
#' 
#' @export
client <- function(remote_addr, port=55555, prompt="remoteR")
{
  assert_that(is.string(remote_addr))
  validate_port(port)
  assert_that(is.string(prompt))
  
  reset_state()
  
  .pbdenv$whoami <- "local"
  .pbdenv$prompt <- prompt
  .pbdenv$port <- port
  .pbdenv$remote_addr <- remote_addr
  
  remoter_repl()
  
  invisible(TRUE)
}

