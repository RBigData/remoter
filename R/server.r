#' Server Launcher
#' 
#' Launcher for the remoter server.
#' 
#' @param port
#' The port (number) that will be used for communication between 
#' the client and server.  The port value for the client and server
#' must agree.
#' @param password
#' A password the client must enter before the user can process
#' commands on the server.  If the value is \code{NULL}, then no
#' password checking takes place.
#' @param maxretry
#' The maximum number of retries for passwords before shutting
#' everything down.
#' @param showmsg
#' Logical; if TRUE, turns on the "debug mode" for the server,
#' and prints messages in the server terminal.
#' 
#' @export
server <- function(port=55555, password=NULL, maxretry=5, showmsg=FALSE)
{
  reset_state()
  
  pbdenv$whoami <- "remote"
  pbdenv$port <- port
  pbdenv$debug <- showmsg
  pbdenv$password <- password
  remoter_repl()
  
  invisible(TRUE)
}

