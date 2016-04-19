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
client <- function(addr="localhost", port=55555, prompt="remoter", timer=FALSE)
{
  assert_that(is.flag(timer))
  assert_that(is.string(prompt))
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(port)
  
  test_connection(addr, port)
  
  reset_state()
  
  set(whoami, "local")
  set(prompt, prompt)
  set(timer, timer)
  set(port, port)
  set(remote_addr, addr)
  
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
  
  repeat
  {
    check <- tryCatch(read <- readline(prompt=prompt), interrupt=function(.) Cc_check)
    if (check != Cc_check)
      break
    else
    {
      cat("^C\n")
    }
  }
  
  ret <- c(input, read)
  ret <- remoter_sanitize(inputs=ret)
  
  return(ret)
}



remoter_sanitize <- function(inputs)
{
  for (i in 1:length(inputs))
  {
    input <- inputs[i]
    if (grepl(x=input, pattern="^(\\s+)?(q|quit)\\(", perl=TRUE)) 
      inputs[i] <- "exit(client.only=TRUE)"
    else if (grepl(x=input, pattern=".pbdenv") && !getval(debug))
    {
      remoter_client_stop("I can't do that.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?geterrmessage\\(", perl=TRUE))
      inputs[i] <- getval(client_lasterror)
    else if (grepl(x=input, pattern="^(\\s+)?(\\?|\\?\\?|help\\()", perl=TRUE))
    {
      remoter_client_stop("Reading help files from the server is currently not supported.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?warnings\\(", perl=TRUE))
    {
      set.status(shouldwarn, TRUE)
      remoter_show_warnings()
      inputs[i] <- "invisible()"
    }
    else if (input == "")
      inputs[i] <- "invisible()"
  }
  
  return(inputs)
}



remoter_client_send <- function(input)
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
  
  set(status, remoter_receive())
  
  ### Must come last! If client only wants to quit, server doesn't know 
  ### about it, and resets the status on receive.socket()
  if (all(grepl(x=input, pattern="^(\\s+)?exit\\(", perl=TRUE)))
    eval(parse(text=input))
  
#    remoter_show_errors()
#    remoter_show_warnings()
  
  invisible()
}



remoter_init_client <- function()
{
  set(context, pbdZMQ::init.context())
  set(socket, pbdZMQ::init.socket(getval(context), "ZMQ_REQ"))
  addr <- pbdZMQ::address(getval(remote_addr), getval(port))
  pbdZMQ::connect.socket(getval(socket), addr)
  
  test <- remoter_check_password_local()
  if (!test) return(FALSE)
  remoter_check_version_local()
  cat("\n")
  
  return(TRUE)
}



remoter_repl_client <- function(env=globalenv())
{
  if (!interactive())
    stop("You can only use the client interactively at this time")
  
  test <- remoter_init_client()
  if (!test) return(FALSE)
  
  timer <- getval(timer)
  if (timer)
    EVALFUN <- function(expr) capture.output(system.time(expr))
  else
    EVALFUN <- identity
  
  while (TRUE)
  {
    input <- character(0)
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      input <- remoter_readline(input=input)
      
      timing <- EVALFUN({
        remoter_client_send(input=input)
        
        if (get.status(continuation)) next
        
        remoter_repl_printer()
      })
      
      if (timer)
        cat(paste0(timing[-1], collapse="\n"), "\n")
      
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
