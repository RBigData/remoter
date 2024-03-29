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
#' @param logfile
#' A file path to use for server logging. If the value is \code{NULL}, then no
#' logging takes place.
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
#' @param serialversion
#' NULL or numeric; the workspace format version to use when serializing.
#' NULL specifies the current default version. The only other supported
#' values are 2 and 3.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
server <- function(port=55555, password=NULL, maxretry=5, secure=has.sodium(),
  logfile=NULL, verbose=FALSE, showmsg=FALSE, userpng=TRUE, sync=TRUE,
  serialversion=NULL)
{
  if (length(port) == 1 && port == 0)
    port <- pbdZMQ::random_open_port()
  
  validate_port(port, warn=TRUE)
  check(is.null(password) || is.string(password))
  check(is.null(logfile) || is.string(logfile))
  check.is.posint(maxretry)
  check.is.flag(secure)
  check.is.flag(verbose)
  check.is.flag(showmsg)
  check.is.flag(userpng)
  check.is.flag(sync)
  check(is.null(serialversion) || is.inty(serialversion))
  
  if (!has.sodium() && secure)
    stop("secure servers can only be launched if the 'sodium' package is installed")
  if (is.null(logfile) && verbose)
    stop("option 'verbose' is enabled without a specified logfile")
  
  serverlog = !is.null(logfile)
  
  if (port == 0)
    port <- pbdZMQ::random_open_port()
  
  reset_state()
  set(whoami, "remote")
  set(logfile, logfile)
  set(serverlog, serverlog)
  set(verbose, verbose)
  set(showmsg, showmsg)
  set(port, port)
  set(secure, secure)
  set(sync, sync)
  set(password, pwhash(password))
  set(serialversion, serialversion)
  
  logfile_init()
  
  ### Backup default device and set the rpng as a defult opening device.
  options(device.default = getOption("device"))
  if (userpng)
    options(device = remoter::rpng)
  
  eval(parse(text = "suppressMessages(library(remoter, quietly = TRUE))"), envir = globalenv()) 
  
  options(warn = 1)
  
  ips = get_ips()
  logprint(paste("*** Launching", ifelse(getval(secure), "secure", "UNSECURE"), "server ***"), preprint="\n", forcemsg=TRUE)
  logprint(paste("    Internal IP: ", ips$ip_in), timestamp=FALSE, forcemsg=TRUE)
  logprint(paste("    External IP: ", ips$ip_ex), timestamp=FALSE, forcemsg=TRUE)
  logprint(paste("    Hostname:    ", get_hostname()), timestamp=FALSE, forcemsg=TRUE)
  logprint(paste("    Port:        ", port), timestamp=FALSE, forcemsg=TRUE)
  logprint(paste("    Version:     ", packageVersion("remoter")), timestamp=FALSE, forcemsg=TRUE)
  
  rm("port", "password", "maxretry", "secure", "logfile", "verbose", "showmsg",
    "userpng", "sync", "serialversion")
  invisible(gc())
  
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


  ### Do the above one more time for ggplot2 ...
  if (!is.null(ret))
  {
    if (ret$visible && is.gg.ggplot(ret$value))
    {
      ### When g is a gg.ggplot object, simply typing `g` just returns from
      ### the earlier call "tryCatch()" generating "additionmsg".
      ### i.e. identity(g) is returned to the "ret$value", no `print(g)` is
      ### executed.

      ### Note that `g <- ggplot(...) + ...` may not check missing objects or
      ### variables carefully at the time generate the object g. Really!!!

      ### i.e. the delay execution until printing for the ggplot object needs
      ### to be dealed with further below with checking. Otherwise, the
      ### `set.status(ret, utils::capture.output(ret$value))` in later call
      ### will break the sever because there is no error handler down there.

      ### Any S3 or S4 print methods can crash similarly if designed badly.
      ### Hopefully, ggplot2 is the only package.
      additionmsg <-
      capture.output({
        sink(file = stdout(), type = "message")
        ret <-
        withCallingHandlers(
          tryCatch({
              withVisible(eval(print(ret$value), envir=env))
            }, interrupt=identity, error=remoter_error
          ), warning=remoter_warning
        )
        sink(file = NULL, type = "message")
      })
    }
  }
  
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
      ### The g below opens an rpng which needs auto_rpng_off_local.
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
