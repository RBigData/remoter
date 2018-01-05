#' Client Launcher
#' 
#' Connect to a remote server (launch the client).
#' 
#' @details
#' The \code{port} values between the client and server must agree.
#' If they do not, this can cause the client to hang.
#' The client is a specialized REPL that intercepts commands sent
#' through the R interpreter.  These commands are then sent from the
#' client to and evaluated on the server.
#' The client communicates over ZeroMQ with the server using a REQ/REP pattern.
#' Both commands (from client to server) and returns (from server
#' to client) are handled in this way.
#' 
#' To shut down the server and the client, see \code{exit()}.
#' 
#' @param addr
#' The remote host/address/endpoint.
#' @param port
#' The port (number) that will be used for communication between 
#' the client and server.  The port value for the client and server
#' must agree.
#' @param password
#' An initial password to pass to the server.  If the server is not accepting
#' passwords, then this argument is ignored.  If the initial pasword is
#' incorrect, then assuming the server's \code{maxretry>1}, then you will be
#' interactively asked to enter the password.
#' @param prompt
#' The prompt to use to delineate the client from the normal R REPL.
#' @param timer
#' Logical; should the "performance prompt", which shows timing
#' statistics after every command, be used?
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
client <- function(addr="localhost", port=55555, password=NULL,
  prompt="remoter", timer=FALSE)
{
  check.is.flag(timer)
  check.is.string(prompt)
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(port, warn=FALSE)
  
  test_connection(addr, port)
  
  reset_state()
  
  set(whoami, "local")
  set(prompt, prompt)
  set(timer, timer)
  set(port, port)
  set(remote_addr, addr)
  set(clientpw, password)
  
  set(isbatch, FALSE)

  ### Both axes are proportionally scaled to fit the new window size
  if (isWindows())
    grDevices::windows.options(rescale = "fit")
  
  remoter_repl_client()
  
  invisible(TRUE)
}



remoter_readline <- function(input)
{
  if (get.status(continuation))
    symb <- "+ "
  else
    symb <- "> "
  
  prompt <- paste0(getval(prompt), symb)
  
  Cc_check <- ".__cantstopwontstop"
  
  Cc_ct <- 1L
  repeat
  {
    check <- tryCatch(read <- readline(prompt=prompt), interrupt=function(.) Cc_check)
    if (check == Cc_check && get.status(continuation))
      return("remoter_client_halt")
    else if (check != Cc_check || Cc_ct >= 3L)
      break
    else
    {
      Cc_ct <- Cc_ct + 1
      cat("^C\n")
    }
  }
  
  if (!exists("read"))
  {
    cat("3 ctrl+c's detected; killing remoter client...\n")
    read <- "exit()"
  }
  
  ### Add to history() and avoid repeatedly appending suffix.
  addhistory(read)

  ret <- c(input, read)
  ret <- remoter_sanitize(inputs=ret)

  return(ret)
}



### TODO use a proper parser...
remoter_sanitize <- function(inputs)
{
  for (i in 1:length(inputs))
  {
    input <- inputs[i]
    if (grepl(x=input, pattern="^(\\s+)?(q|quit)\\(", perl=TRUE)) 
      inputs[i] <- "exit(client.only=TRUE)"
    else if (grepl(x=input, pattern="(.pbdenv|remoter:::)", perl=TRUE) && !getval(debug))
    {
      remoter_client_stop("I can't do that.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?geterrmessage\\(", perl=TRUE))
      inputs[i] <- getval(client_lasterror)
    else if (grepl(x=input, pattern="^(\\s+)?(\\?\\?|help.search\\(|help.start\\()", perl=TRUE))
    {
      remoter_client_stop("Using help() to obtain help files from the server.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?debug\\(", perl=TRUE))
    {
      remoter_client_stop("debug mode is currently not supported.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?warnings\\(", perl=TRUE))
    {
      set.status(shouldwarn, TRUE)
      remoter_show_warnings(force=TRUE)
      inputs[i] <- "invisible()"
    }
    else if (input == "")
      inputs[i] <- "invisible()"
    else if (grepl(x=input, pattern="^(\\s+)?(remoter::)?(client|server|relay|batch)\\(", perl=TRUE))
    {
      remoter_client_stop("can not spawn client/server/relay or launch a batch connection from inside the client")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="remoter_client_halt"))
      inputs[i] <- "invisible()"
  }
  
  return(inputs)
}



remoter_client_sendrecv <- function(input, env)
{
  remoter_send(data=input)
  
  ### Special cases that need to be eval'd locally
  if (all(grepl(x=input, pattern="^(\\s+)?s2c\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?c2s\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?lsc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?rmc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?evalc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.curc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.listc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.nextc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.prevc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.offc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.setc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.newc\\(", perl=TRUE)))
    eval(parse(text=input))
  else if (all(grepl(x=input, pattern="^(\\s+)?dev.sizec\\(", perl=TRUE)))
    eval(parse(text=input))
  
  ### Update status by the server's results.
  set(status, remoter_receive())
  
  ### Update client's local env as necessary
  remote_objs <- get.status(remote_objs)
  if (!is.null(remote_objs))
  {
    for (nm in ls(envir=remote_objs))
      assign(nm, get(nm, envir=remote_objs), envir=env)
  }
  
  ### Because rpng.off() needs a call at the client to display image.
  if (get.status(need_auto_rpng_off))
    auto_rpng_off_local(get.status(ret))
  
  ### Because rhelp() needs a call at the client to cast help message.
  if (get.status(need_auto_rhelp_on))
    auto_rhelp_on_local(get.status(ret))
  
  ### Must come last! If client only wants to quit, server doesn't know 
  ### about it, and resets the status on receive.socket()
  if (all(grepl(x=input, pattern="^(\\s+)?exit\\(", perl=TRUE)))
    eval(parse(text=input))
  
  invisible()
}



remoter_init_client <- function()
{
  set(context, pbdZMQ::init.context())
  set(socket, pbdZMQ::init.socket(getval(context), "ZMQ_REQ"))
  addr <- pbdZMQ::address(getval(remote_addr), getval(port))
  pbdZMQ::connect.socket(getval(socket), addr)
  
  test <- remoter_check_password_local()
  if (!test)
    return(FALSE)
  
  remoter_check_version_local()
  cat("\n")
  
  return(TRUE)
}



timerfun <- function(timer)
{
  if (timer)
    EVALFUN <- function(expr) capture.output(system.time(expr))
  else
    EVALFUN <- identity
  
  EVALFUN
}



timerprint <- function(timer, timing)
{
  if (timer)
    cat(paste0(timing[-1], collapse="\n"), "\n")
  
  invisible()
}



remoter_client_objcleanup <- function(env)
{
  names <- ls(envir=env)
  names <- names[grep("_REMOTE", names)]
  rm(list=names, envir=env)
  
  invisible()
}



remoter_repl_client <- function(env=globalenv())
{
  if (!interactive())
    stop("You can only use the client interactively. Use bacth() to execute in batch.")
  
  test <- remoter_init_client()
  if (!test)
    return(FALSE)
  
  timer <- getval(timer)
  EVALFUN <- timerfun(timer)
  
  # a bit of a hack, but we pass a dumb message to the server to sync environments
  remoter_client_sendrecv(input="remoter_env_sync", env=env)
  
  while (TRUE)
  {
    input <- character(0)
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      input <- remoter_readline(input=input)
      if (identical(input[length(input)], "remoter_client_halt"))
        break

      timing <- EVALFUN({
        remoter_client_sendrecv(input=input, env=env)
      })
      
      if (get.status(continuation)) next
      
      remoter_repl_printer()
      
      timerprint(timer, timing)
      
      if (get.status(should_exit))
      {
        remoter_client_objcleanup(env)
        return(invisible())
      }
      
      break
    }
  }
  
  
  remoter_client_objcleanup(env)
  
  return(invisible())
}
