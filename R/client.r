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
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
client <- function(addr="localhost", port=55555, prompt="remoteR")
{
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(port)
  assert_that(is.string(prompt))
  
  test_connection(addr, port)
  
  reset_state()
  
  set(whoami, "local")
  set(prompt, prompt)
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
  
  prompt <- paste0(.pbdenv$prompt, symb)
  
  Cc_check <- ".__cantstopwontstop"
  
  repeat
  {
    check <- tryCatch(read <- readline(prompt=prompt), interrupt=function(.) Cc_check)
    if (check != Cc_check)
      break
    else
    {
      cat("C-c\n")
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
    else if (grepl(x=input, pattern=".pbdenv") && !.pbdenv$debug)
    {
      remoter_client_stop("I can't do that.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?geterrmessage\\(", perl=TRUE))
      inputs[i] <- .pbdenv$client_lasterror
    else if (grepl(x=input, pattern="^(\\s+)?(\\?|\\?\\?|help\\()", perl=TRUE))
    {
      remoter_client_stop("Reading help files from the server is currently not supported.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^(\\s+)?warnings\\(", perl=TRUE))
    {
      .pbdenv$status$shouldwarn <- TRUE
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
  send(data=input)
  
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
  
  .pbdenv$status <- receive()
  
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
  .pbdenv$context <- init.context()
  .pbdenv$socket <- init.socket(.pbdenv$context, "ZMQ_REQ")
  addr <- pbdZMQ::address(.pbdenv$remote_addr, .pbdenv$port)
  connect.socket(.pbdenv$socket, addr)
  
  remoter_check_password_local()
  remoter_check_version_local()
  cat("\n")
  
  return(TRUE)
}



remoter_repl_client <- function(env=sys.parent())
{
  if (!interactive())
    stop("You can only use the client interactively at this time")
  
  remoter_init_client()
  
  while (TRUE)
  {
    input <- character(0)
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      .pbdenv$visible <- withVisible(invisible())
      input <- remoter_readline(input=input)
      
      remoter_client_send(input=input)
      
      if (get.status(continuation)) next
      
      remoter_repl_printer()
      
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
