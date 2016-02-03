#' Client Launcher
#' 
#' Connect to a remote server (launch the client).
#' 
#' @param addr
#' The remote host/address/endpoint.
#' @param port
#' The port (number) that will be used for communication between 
#' the client and server.  The port value for the client and server
#' must agree.
#' @param prompt
#' The prompt to use to delineate the client from the normal R REPL.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @details
#' The \code{port} values between the client and server must agree.
#' If they do not, this can cause the client to hang.
#' 
#' The client is a specialized REPL that intercepts commands sent
#' through the R interpreter.  These commands are then sent from the
#' client to and evaluated on the server.
#' The client communicates over ZeroMQ with the server using a REQ/REP pattern.
#' Both commands (from client to server) and returns (from server
#' to client) are handled in this way.
#' 
#' To shut down the server and the client, see \code{exit()}.
#' 
#' @export
client <- function(addr="localhost", port=55555, prompt="remoteR")
{
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(port)
  assert_that(is.string(prompt))
  
  test_connection(addr, port)
  
  reset_state()
  
  set(whoami, "local")
  set(prompt, prompt)
  set(port, port)
  set(remote_addr, addr)
  
  remoter_repl()
  
  invisible(TRUE)
}
