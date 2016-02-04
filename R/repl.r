### For R CMD check
utils::globalVariables(c("continuation", "lasterror", "visible", "should_exit", "remoter_prompt_active", "ret", "lasterror"))



remoter_readline <- function(input)
{
  if (get.status(continuation))
    symb <- "+ "
  else
    symb <- "> "
  
  prompt <- paste0(.pbdenv$prompt, symb)
  
  if (iam("local"))
  {
    ret <- c(input, readline(prompt=prompt))
    ret <- remoter_sanitize(inputs=ret)
  }
  else
    ret <- NULL
  
  return(ret)
}



remoter_client_stop <- function(msg)
{
  .pbdenv$client_lasterror <- msg
  cat("Error: ", msg, "\n")
  
  invisible()
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



is.error <- function(obj)
{
  if (inherits(obj, "try-error") || inherits(obj, "error"))
    return(TRUE)
  else
    return(FALSE)
}



remoter_repl_printer <- function()
{
  if (iam("remote"))
  {
    .pbdenv$status$shouldwarn <- FALSE
    return(invisible())
  }
  
  if (get.status(visible))
    cat(paste(get.status(ret), collapse="\n"), "\n")
  
  remoter_show_errors()
  remoter_show_warnings()
  
  return(invisible())
}



remoter_interrupt <- function(x)
{
#  .pbdenv$status$remoter_prompt_active <- TRUE
  cat("interrupt\n")
  print(x)
}



remoter_warning <- function(warn)
{
  .pbdenv$status$shouldwarn <- TRUE
  .pbdenv$status$num_warnings <- .pbdenv$status$num_warnings + 1
  
  .pbdenv$status$warnings <- append(.pbdenv$status$warnings, conditionMessage(warn))
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



remoter_show_errors <- function()
{
  if (!is.null(get.status(lasterror)))
    cat(get.status(lasterror))
  
  invisible()
}



remoter_show_warnings <- function()
{
  warnings <- get.status(warnings)
  nwarnings <- length(warnings)
  
  if (!is.null(warnings) && .pbdenv$status$shouldwarn)
  {
    if (nwarnings == 1)
    {
      cat("Warning message:\n")
      cat(warnings)
    }
    else if (nwarnings < 11)
    {
      cat("Warning messages:\n")
      for (i in 1:nwarnings)
      {
        w <- warnings[i]
        cat(paste0(i, ": ", w, "\n"))
      }
    }
    else
    {
      cat(paste("There were", nwarnings, "warnings (use warnings() to see them)"))
    }
    cat("\n")
  }
  
  .pbdenv$status$shouldwarn <- FALSE
  
  invisible()
}



### TODO FIXME integrate with remoter_sanitize()
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



remoter_eval <- function(input, env)
{
  set.status(continuation, FALSE)
  set.status(lasterror, NULL)
  
  if (iam("local"))
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
  }
  else if (iam("remote"))
  {
    if (.pbdenv$debug)
      cat("Awaiting message:  ")
    
    msg <- receive()
    
    if (.pbdenv$debug)
    {
      if (length(msg)==1 && msg != magicmsg_first_connection)
        cat(msg)
      else
        cat("\r", paste0(rep(" ", 20), collapse=""))
      
      cat("\n")
    }
    
    ### Run first-time checks
    if (length(msg)==1 && msg == magicmsg_first_connection)
    {
      remoter_check_password()
      remoter_check_version()
      return(invisible())
    }
    
    msg <- remoter_eval_filter_server(msg=msg)
    
    ret <- 
    withCallingHandlers(
      tryCatch({
          .pbdenv$visible <- withVisible(eval(parse(text=msg), envir=env))
        }, interrupt=remoter_interrupt, error=remoter_error
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
    
    send(.pbdenv$status)
  }
  else
    stop("bad 'whoami'")
}



remoter_read_password <- function()
{
  # tt <- tktoplevel() 
  # pw <- tclVar("") 
  # 
  # label <- tklabel(tt, text="enter the password") 
  # textbox <- tkentry(tt, show="*", textvariable=pw) 
  # tkbind(textbox, "<Return>", function() tkdestroy(tt)) 
  # button <- tkbutton(tt,text="ok", default="active", command=function() tkdestroy(tt)) 
  # tkpack(label, textbox, button) 
  # 
  # tkwait.window(tt) 
  # 
  # return(tclvalue(pw)) 
  pw <- readline("enter the password:  ") 
  
  pw
}



remoter_check_password <- function()
{
  if (iam("local"))
  {
    first_connect()
    needpw <- receive()
    
    while (needpw)
    {
      pw <- remoter_read_password()
      send(pw)
      check <- receive()
      
      if (isTRUE(check))
        break
      else if (is.null(check))
        stop("Max attempts reached; killing server...")
      
      cat("Sorry, try again.\n")
    }
  }
  else if (iam("remote"))
  {
    if (is.null(.pbdenv$password))
    {
      logprint("client connected")
      send(FALSE)
    }
    else
    {
      logprint("client attempting to connect...")
      send(TRUE)
      
      attempts <- 2L
      while (TRUE)
      {
        pw <- receive()
        if (pw == .pbdenv$password)
        {
          logprint("client connected")
          send(TRUE)
          break
        }
        else if (attempts <= .pbdenv$maxattempts)
          send(FALSE)
        else
        {
          send(NULL)
          stop("Max attempts reached; killing self.")
        }
        
        attempts <- attempts + 1L
      }
    }
  }
}



remoter_check_version <- function()
{
  if (iam("local"))
  {
    send("")
    versions_server <- receive()
    
    if (!isTRUE(versions_server))
    {
      versions_client <- get_versions()
      if (!compare_versions(versions_client, versions_server))
        stop("Incompatible package versions; quitting client (perhaps you need to update and restart the server?)")
    }
  }
  else if (iam("remote"))
  {
    receive()
    
    if (!.pbdenv$checkversion)
      send(FALSE)
    else
    {
      versions <- get_versions()
      send(versions)
    }
  }
}



remoter_repl_init <- function()
{
  generate_keypair()
  
  
  ### Initialize zmq
  if (iam("local"))
  {
    .pbdenv$context <- init.context()
    .pbdenv$socket <- init.socket(.pbdenv$context, "ZMQ_REQ")
    addr <- pbdZMQ::address(.pbdenv$remote_addr, .pbdenv$port)
    connect.socket(.pbdenv$socket, addr)
    
    remoter_check_password()
    remoter_check_version()
    cat("\n")
  }
  else if (iam("remote"))
  {
    ### Order very much matters!
    if (.pbdenv$debug)
      cat("Hello! This is the server; please don't type things here!\n\n")
    
    ### client/server
    .pbdenv$context <- init.context()
    .pbdenv$socket <- init.socket(.pbdenv$context, "ZMQ_REP")
    bind.socket(.pbdenv$socket, paste0("tcp://*:", .pbdenv$port))
  }
  
  return(TRUE)
}



remoter_repl <- function(env=sys.parent())
{
  if (!interactive() && iam("local"))
    stop("You should only use this interactively")
  
  
  remoter_repl_init()
  
  
  ### the repl
  while (TRUE)
  {
    input <- character(0)
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      .pbdenv$visible <- withVisible(invisible())
      input <- remoter_readline(input=input)
      
      remoter_eval(input=input, env=env)
      
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
