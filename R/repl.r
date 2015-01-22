### TODO:
  # -warnings
  # -handle C-c somehow ???
  # -discover source of package loading hang

library(rzmq)


### Global data
pbdenv <- new.env()

pbdenv$prompt <- "pbdR"
pbdenv$pbd_prompt_active <- FALSE
pbdenv$should_exit <- FALSE
pbdenv$continuation <- FALSE

pbdenv$port <- 5555
pbdenv$socket <- NULL

pbdenv$lasterror <- NULL
pbdenv$num_warnings <- 0
pbdenv$warnings <- NULL




###
pbd_readline <- function(input, continuation)
{
  if (continuation)
    symb <- "+ "
  else
    symb <- "> "
  
  prompt <- paste0(pbdenv$prompt, symb)
  
  if (pbdenv$whoami == "local")
  {
    ret <- c(input, readline(prompt=prompt))
    ret <- pbd_sanitize(inputs=ret)
  }
  else
    ret <- NULL
  
  return(ret)
}



pbd_sanitize <- function(inputs)
{
  for (i in 1:length(inputs))
  {
    input <- inputs[i]
    if (grepl(x=input, pattern="(^q\\(|^quit\\()", perl=TRUE)) 
      inputs[i] <- "pbd_exit()"
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



pbd_repl_printer <- function(ret)
{
  if (pbdenv$whoami == "remote") return(invisible())
  
#  cat("----------------------------\n")
#  print(ret)
#  cat("----------------------------\n")
  
  if (!is.null(ret))
  {
    if (ret$visible)
      print(ret$value)
    
    if (pbdenv$num_warnings > 0)
    {
      if (pbdenv$num_warnings > 10)
        cat(paste("There were", pbdenv$num_warnings, "warnings (use warnings() to see them)\n"))
      else
        print(warnings())
    }
  }
  
  return(invisible())
}



pbd_interrupt <- function(x)
{
  pbdenv$pbd_prompt_active <- FALSE
  cat("interrupt\n")
  print(x)
}



pbd_warning <- function(warn)
{
  pbdenv$num_warnings <- pbdenv$num_warnings + 1
  
  append(pbdenv$warnings, conditionMessage(warn))
  invokeRestart("muffleWarning")
  print(warn)
}



pbd_error <- function(err)
{
  msg <- err$message
  pbdenv$continuation <- grepl(msg, pattern="unexpected end of input")
  
  if (!pbdenv$continuation)
  {
    msg <- sub(x=msg, pattern=" in eval\\(expr, envir, enclos\\) ", replacement="")
    pbdenv$lasterror <- paste0("Error: ", msg, "\n")
  }
  else
    pbdenv$lasterror <- NULL
  
  return(invisible())
}



pbd_eval <- function(input, whoami, env)
{
  pbdenv$continuation <- FALSE
  
  if (whoami == "local")
  {
    send.socket(pbdenv$socket, data=input)
    
    ret <- receive.socket(pbdenv$socket)
    pbdenv$continuation <- receive.socket(pbdenv$socket)
    pbdenv$lasterror <- receive.socket(pbdenv$socket)
    if (!is.null(pbdenv$lasterror)) cat(pbdenv$lasterror)
  }
  else if (whoami == "remote")
  {
    cat("Awaiting message:  ")
    msg <- receive.socket(pbdenv$socket)
    cat(msg, "\n")
    
    ret <- withCallingHandlers(
      tryCatch({
          pbdenv$visible <- withVisible(eval(parse(text=msg), envir=env))
        }, interrupt=pbd_interrupt, error=pbd_error
      ), warning=pbd_warning
    )
    
    if (!is.null(ret) && !ret$visible)
      ret <- NULL
    
    send.socket(pbdenv$socket, ret, send.more=TRUE)
    send.socket(pbdenv$socket, pbdenv$continuation, send.more=TRUE)
    send.socket(pbdenv$socket, pbdenv$lasterror)
  }
  else
    stop("bad 'whoami'")
  
  return(ret)
}



pbd_exit <- function()
{
  pbdenv$should_exit <- TRUE
  
  return(invisible())
}



pbd_repl_init <- function()
{
  if (!pbdenv$pbd_prompt_active)
    pbdenv$pbd_prompt_active <- TRUE
  else
  {
    cat("The pbd repl is already running!\n")
    return(FALSE)
  }
  
  ### Initialize zmq
  if (pbdenv$whoami == "local")
  {
    context <- init.context()
    
    socket <- init.socket(context, "ZMQ_REQ")
    pbdenv$socket <- socket
    
    connect.socket(socket, paste0("tcp://localhost:", pbdenv$port))
  }
  else if (pbdenv$whoami == "remote")
  {
    context <- init.context()
    
    socket <- init.socket(context,"ZMQ_REP")
    pbdenv$socket <- socket
    
    bind.socket(socket, paste0("tcp://*:", pbdenv$port))
  }
  
  return(TRUE)
}



pbd_repl <- function(env=sys.parent())
{
  ### FIXME needed?
  if (!interactive() && pbdenv$whoami == "local")
  {
    stop("You should only use this interactively")
  }
  
  
  init <- pbd_repl_init()
  if (!init) return(invisible())
  
  
  ### the repl
  while (TRUE)
  {
    input <- character(0)
    pbdenv$continuation <- FALSE
    
    while (TRUE)
    {
      pbdenv$visible <- withVisible(invisible())
      input <- pbd_readline(input=input, continuation=pbdenv$continuation)
      
      ret <- pbd_eval(input=input, whoami=pbdenv$whoami, env=env)
      
      if (pbdenv$continuation) next
      
      pbd_repl_printer(ret)
      
      ### Should go after all other evals and handlers
      if (pbdenv$should_exit)
      {
        pbdenv$pbd_prompt_active <- pbdenv$should_exit <- FALSE
        return(invisible())
      }
      
      break
    }
  }
  
  pbdenv$pbd_prompt_active <- pbdenv$should_exit <- FALSE
  return(invisible())
}


