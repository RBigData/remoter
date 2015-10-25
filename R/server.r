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
#' @param checkversions
#' Logical; should a version check (pbdZMQ and remoter) be enforced?
#' 
#' @export
server <- function(port=55555, password=NULL, maxretry=5, checkversions=TRUE, showmsg=FALSE)
{
  validate_port(port)
  if (port < 49152)
    warning("See '?pbdZMQ::random_port'")
  assert_that(is.null(password) || is.string(password))
  assert_that(is.infinite(maxretry) || is.count(maxretry))
  assert_that(is.logical(showmsg))
  assert_that(is.logical(checkversions))
  
  reset_state()
  
  .pbdenv$whoami <- "remote"
  .pbdenv$port <- port
  .pbdenv$debug <- showmsg
  .pbdenv$password <- password
  .pbdenv$checkversion <- checkversion
  
  rm("port", "password", "maxretry", "showmsg")
  invisible(gc())
  
  remoter_repl()
  
  invisible(TRUE)
}

