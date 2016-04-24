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
  
  logprint(paste("*** Launching", ifelse(getval(secure), "secure", "UNSECURE"), "server ***"), preprint="\n")
  
  rm("port", "password", "maxretry", "showmsg", "secure", "log", "verbose")
  invisible(gc())
  
  eval(parse(text = "library(remoter, quietly = TRUE)"), envir = globalenv()) 
  
  remoter_repl_server()
  remoter_exit_server()
  
  invisible(TRUE)
}



remoter_warning <- function(warn)
{
  set.status(shouldwarn, TRUE)
  set.status(num_warnings, get.status(num_warnings) + 1L)
  
  set.status(warnings, append(get.status(warnings), conditionMessage(warn)))
  invokeRestart("muffleWarning")
  print(warn)
}



remoter_error <- function(err)
{
  msg <- err$message
  set.status(continuation, grepl(msg, pattern="unexpected end of input"))
  
  if (!get.status(continuation))
  {
    msg <- sub(x=msg, pattern=" in eval\\(expr, envir, enclos\\) ", replacement="")
    set.status(lasterror, paste0("Error: ", msg, "\n"))
  }
  
  return(invisible())
}



remoter_eval_filter_server <- function(msg)
{
  if (all(grepl(x=msg, pattern="^(\\s+)?library\\(", perl=TRUE)))
  {
    msg <- paste0("
      tmp <- file(tempfile())
      sink(tmp, append=TRUE)
      sink(tmp, append=TRUE, type='message')\n", 
      msg, "\n
      sink()
      sink(type='message')
      cat(paste(readLines(tmp), collapse='\n'))
      unlink(tmp)
    ")
  }
  
  msg
}



remoter_server_eval <- function(env)
{
  set.status(continuation, FALSE)
  set.status(lasterror, NULL)
  
  msg <- remoter_receive()
  
  logprint(level="RMSG", msg[length(msg)], checkshowmsg=TRUE)
  
  ### Run first-time checks
  if (length(msg)==1 && msg == magicmsg_first_connection)
  {
    test <- remoter_check_password_remote()
    if (!test) return(invisible())
    remoter_check_version_remote()
    return(invisible())
  }
  
  msg <- remoter_eval_filter_server(msg=msg)
  
  ret <- 
  withCallingHandlers(
    tryCatch({
        withVisible(eval(parse(text=msg), envir=env))
      }, interrupt=identity, error=remoter_error
    ), warning=remoter_warning
  )
  
  if (!is.null(ret))
  {
    set.status(visible, ret$visible)
    
    if (!ret$visible)
      set.status(ret, NULL)
    else
      set.status(ret, utils::capture.output(ret$value))
  }
  
  remoter_send(getval(status))
}



remoter_init_server <- function()
{
  set(context, pbdZMQ::init.context())
  set(socket, pbdZMQ::init.socket(getval(context), "ZMQ_REP"))
  pbdZMQ::bind.socket(getval(socket), pbdZMQ::address("*", getval(port)))
  
  return(TRUE)
}



remoter_exit_server <- function()
{
  if (getval(kill_interactive_server))
    q("no")
  
  return(TRUE)
}



remoter_repl_server <- function(env=globalenv(), initfun=remoter_init_server, evalfun=remoter_server_eval)
{
  initfun()
  
  while (TRUE)
  {
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      
      evalfun(env=env)
      
      if (get.status(continuation)) next
      
      ### Should go after all other evals and handlers
      if (get.status(should_exit))
      {
        set.status(remoter_prompt_active, FALSE)
        set.status(should_exit, FALSE)
        
        return(invisible())
      }
      
      break
    }
  }
  
  set.status(remoter_prompt_active, FALSE)
  set.status(should_exit, FALSE)
  
  return(invisible())
}
