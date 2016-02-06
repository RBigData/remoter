#' Server Launcher
#' 
#' Launcher for the remoter server.
#' 
#' @description
#' TODO
#' 
#' @param port
#' The port (number) that will be used for communication between 
#' the client and server.  The port value for the client and server
#' must agree.
#' @param log
#' Logical; enables some basic logging in the server.
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
#' @param secure
#' Logical; TODO FIXME
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
server <- function(port=55555, log=TRUE, password=NULL, maxretry=5, checkversions=TRUE, showmsg=FALSE, secure=has.sodium())
{
  validate_port(port)
  assert_that(is.flag(log))
  assert_that(is.null(password) || is.string(password))
  assert_that(is.infinite(maxretry) || is.count(maxretry))
  assert_that(is.flag(checkversions))
  assert_that(is.flag(showmsg))
  assert_that(is.flag(secure))
  
  reset_state()
  
  set(whoami, "remote")
  set(whoami, "remote")
  set(serverlog, log)
  set(port, port)
  set(debug, showmsg)
  set(password, password)
  set(checkversion, checkversions)
  
  set(secure, secure)
  if (!.pbdenv$withsodium && secure)
    stop("need salt")
  
  logprint(paste("*** Launching", ifelse(.pbdenv$secure, "secure", "UNSECURE"), "server ***\n"))
  
  rm("port", "password", "maxretry", "showmsg", "secure")
  invisible(gc())
  
  remoter_repl()
  
  invisible(TRUE)
}
