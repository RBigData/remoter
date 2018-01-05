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
#' must agree.  If the value is 0, then a random open port will be
#' selected.
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
#' Logical; if TRUE, messages from the client are logged.
#' @param userpng
#' Logical; if TRUE, rpng is set as the default device for displaying.
#' @param sync
#' Logical; if TRUE, the client will have \code{str()}'d versions of server
#' objects recreated in the global environment.  This is useful in IDE's like
#' RStudio, but it carries a performance penalty.  For terminal users, this is
#' not recommended.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
server <- function(port=55555, password=NULL, maxretry=5, secure=has.sodium(),
  log=TRUE, verbose=FALSE, showmsg=FALSE, userpng=TRUE, sync=TRUE)
{
  if (length(port) == 1 && port == 0)
    port <- pbdZMQ::random_open_port()
  
  validate_port(port, warn=TRUE)
  check(is.null(password) || is.string(password))
  check.is.posint(maxretry)
  check.is.flag(secure)
  check.is.flag(log)
  check.is.flag(verbose)
  check.is.flag(showmsg)
  check.is.flag(userpng)
  check.is.flag(sync)
  
  if (!log && verbose)
    log <- TRUE
  
  if (!has.sodium() && secure)
    stop("secure servers can only be launched if the 'sodium' package is installed")
  
  reset_state()
  
  if (port == 0)
    port <- pbdZMQ::random_open_port()
  
  set(whoami, "remote")
  set(logfile, logfile_init())
  set(serverlog, log)
  set(verbose, verbose)
  set(showmsg, showmsg)
  set(port, port)
  set(secure, secure)
  set(sync, sync)
  set(password, pwhash(password))
  
  ### Backup default device and set the rpng as a defult opening device.
  options(device.default = getOption("device"))
  if (userpng)
    options(device = remoter::rpng)
  

  eval(parse(text = "suppressMessages(library(remoter, quietly = TRUE))"), envir = globalenv()) 
  
  options(warn = 1)
  
  
  logprint(paste("*** Launching", ifelse(getval(secure), "secure", "UNSECURE"), "server ***"), preprint="\n")
  ### TODO
  # ips <- remoter_getips()
  # logprint(paste("                           Internal IP: ", ips$ip_in), timestamp=FALSE)
  # logprint(paste("                           External IP: ", ips$ip_ex), timestamp=FALSE)
  logprint(paste("                           Hostname:    ", get_hostname()), timestamp=FALSE)
  logprint(paste("                           Port:        ", port), timestamp=FALSE)
  
  rm("port", "password", "maxretry", "showmsg", "secure", "log", "verbose", "userpng")
  invisible(gc())
  
  remoter_repl_server()
  remoter_exit_server()
  
  invisible(TRUE)
}



### TODO
# remoter_getips <- function()
# {
#   ip_in <- tryCatch(getip::getip("internal"), error=identity)
#   if (inherits(tryCatch(ip_in, error=identity), "error"))
#     ip_in  <- "ERROR: couldn't determine internal IP"
#   
#   ip_ex <- tryCatch(getip::getip("external"), error=identity)
#   if (inherits(tryCatch(ip_ex, error=identity), "error"))
#     ip_ex  <- "ERROR: couldn't determine external IP"
#   
#   return(list(ip_in=ip_in, ip_ex=ip_ex))
# }



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
  set.status(continuation, grepl(msg, pattern="(unexpected end of input|unexpected INCOMPLETE_STRING)"))
  
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



remoter_server_check_objs <- function(env, force=FALSE)
{
  if (!getval(sync))
    return(invisible())
  
  objs_nm_new <- ls(envir=env)
  if (force || !identical(getval(objs_nm), objs_nm_new))
  {
    set(objs_nm, objs_nm_new)
    for (nm in objs_nm_new)
      assign(paste0(nm, "_REMOTE"), capture.output(str(get(nm, envir=env))), envir=getval(objs))
  }
  
  set.status(remote_objs, getval(objs))
}



remoter_server_eval <- function(env)
{
  set.status(shouldwarn, FALSE)
  set.status(continuation, FALSE)
  set.status(lasterror, NULL)
  set.status(need_auto_rpng_off, FALSE)
  set.status(need_auto_rhelp_on, FALSE)
  set.status(remote_objs, NULL)
  
  
  msg <- remoter_receive()
  
  logprint(level="RMSG", msg[length(msg)], checkshowmsg=TRUE)
  
  ### Run first-time checks
  if (length(msg)==1 && msg == magicmsg_first_connection)
  {
    test <- remoter_check_password_remote()
    if (!test) 
      return(invisible())
    
    remoter_check_version_remote()
    return(invisible())
  }
  
  msg <- remoter_eval_filter_server(msg=msg)
  
  ### Sync environment if necessary
  if (length(msg) == 1 && msg == "remoter_env_sync")
  {
    remoter_server_check_objs(env, force=TRUE)
    remoter_send(getval(status))
    return(invisible())
  }
  
  
  ### Divert/sink `R message` (warning, error, stop) to stdout
  additionmsg <-
  capture.output({
    sink(file = stdout(), type = "message")
    ret <-
    withCallingHandlers(
      tryCatch({
          withVisible(eval(parse(text=msg), envir=env))
        }, interrupt=identity, error=remoter_error
      ), warning=remoter_warning
    )
    sink(file = NULL, type = "message")
  })
  
  
  ### Handle log printing for exit()/shutdown(): NOTE must happen outside of eval since we capture all output now
  if (getval(client_called_shutdown) == TRUE)
    logprint("client killed server")
  else if (getval(client_called_exit) == TRUE)
  {
    logprint("client disconnected with call to exit()")
    set(client_called_exit, FALSE)
  }
  
  
  ### Take care the `R output` from ret.
  if (!is.null(ret))
  {
    set.status(visible, ret$visible)
    
    if (!ret$visible)
      set.status(ret, NULL)
    else
      set.status(ret, utils::capture.output(ret$value))

    ### The output of this if is an image from a S3 method via print.ggplot().
    if (ret$visible && is.gg.ggplot(ret$value) && is.rpng.open())
    {
      ### The g below opens anrpng which needs auto_rpng_off_local.
      ### remoter> g <- ggplot(da, aes(x, y)) + geom_point()
      ### remoter> g
      ret$value <- rpng.off()
      ret$visible <- FALSE
    }
    
    ### The output of this if is an image from dev.off().
    if (get.status(need_auto_rpng_off))
    {
      set.status(ret, ret$value)
      set.status(visible, ret$visible)
    }
    
    ### The output is an Rd from help().
    if (get.status(need_auto_rhelp_on))
    {
      set.status(ret, ret$value)
      set.status(visible, FALSE)
    }
  }
  
  ### Take care the `R output` from cat/print/message
  if (length(additionmsg) == 0)
    set.status(ret_addition, NULL)
  else 
  {
    set.status(ret_addition, additionmsg)
    ### Print to server if needed for debugging
    if (getval(verbose))
      cat(additionmsg, sep = "\n")
  }
  
  
  remoter_server_check_objs(env)
  
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
      
      if (get.status(should_exit))
        return(invisible())
      
      break
    }
  }
  
  return(invisible())
}
