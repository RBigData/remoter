#' Client/Server Launchers
#' 
#' Launchers for the remoter client/server.
#' 
#' @param port
#' The port (number) that will be used for communication between 
#' the client and server.
#' @param showmsg
#' Logical; if TRUE, turns on the "debug mode" for the server,
#' and prints messages in the server terminal.
#' @param remote_addr
#' The remote host/address/endpoint.
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
#' @name launchers
#' @rdname launchers
NULL



#' @rdname launchers
#' @export
server <- function(port=55555, showmsg=FALSE)
{
  pbdenv$whoami <- 'remote'
  pbdenv$port <- port
  pbdenv$debug <- showmsg
  remoter_repl()
  
  invisible(TRUE)
}



#' @rdname launchers
#' @export
client <- function(remote_addr, port=55555)
{
  pbdenv$whoami <- "local"
  pbdenv$port <- port
  pbdenv$remote_addr <- remote_addr
  remoter_repl()
  
  invisible(TRUE)
}

