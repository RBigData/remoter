#' Server Launcher
#' 
#' Launcher for the remoter server.
#' 
#' @details
#' By a 'secure' server, we mean one that encrypts messages it
#' sends and only accepts encrypted messages.  Encryption uses
#' public key cryptography, using the 'sodium' package.
#' 
#' If the 'sodium' package is available to the server, then by 
#' default the server will be secure.  If the package is not
#' available, then you will not be able to start a secure server.
#' If the server is secure, then a client can only connect if
#' the client has the 'sodium' package available.
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
#' @param secure
#' Logical; enables encryption via public key cryptography of
#' the 'sodium' package is available.
#' @param log
#' Logical; enables some basic logging in the server.
#' @param verbose
#' Logical; enables the verbose logger.
#' @param showmsg
#' Logical; if TRUE, messages from the client are logged
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
server <- function(port=55555, password=NULL, maxretry=5, secure=has.sodium(), log=TRUE, verbose=FALSE, showmsg=FALSE)
{
  validate_port(port)
  assert_that(is.null(password) || is.string(password))
  assert_that(is.infinite(maxretry) || is.count(maxretry))
  assert_that(is.flag(secure))
  assert_that(is.flag(log))
  assert_that(is.flag(verbose))
  assert_that(is.flag(showmsg))
  
  if (!log && verbose)
  {
    warning("logging must be enabled for verbose logging! enabling logging...")
    log <- TRUE
  }
  
  if (!has.sodium() && secure)
    stop("secure servers can only be launched if the 'sodium' package is installed")
  
  reset_state()
  
  set(whoami, "remote")
  set(serverlog, log)
  set(verbose, verbose)
  set(showmsg, showmsg)
  set(port, port)
  set(password, password)
  set(secure, secure)
  set(logfile, logfile_init())
  
  logprint(paste("*** Launching", ifelse(.pbdenv$secure, "secure", "UNSECURE"), "server ***"), preprint="\n\n")
  
  rm("port", "password", "maxretry", "showmsg", "secure")
  invisible(gc())
  
  remoter_repl_server()
  
  invisible(TRUE)
}
