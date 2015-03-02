### Global data
pbdenv <- new.env()

# options
pbdenv$prompt <- "pbdR"
pbdenv$port <- 5555
pbdenv$remote_port <- 5556
pbdenv$bcast_method <- "zmq"


# internals
pbdenv$context <- NULL
pbdenv$socket <- NULL

pbdenv$remote_context <- NULL
pbdenv$remote_socket <- NULL

pbdenv$status <- list(
  ret               = invisible(),
  visible           = FALSE,
  lasterror         = NULL,
  num_warnings      = 0,
  warnings          = NULL,
  pbd_prompt_active = FALSE,
  should_exit       = FALSE,
  continuation      = FALSE
)

pbdenv_checker <- function()
{
  if (pbdenv$bcast_method != "mpi" && pbdenv$bcast_method != "zmq")
    stop("'bcast_method' must be one of 'mpi' or 'zmq'")
  
  return(TRUE)
}

### just a pinch of sugar
get.status <- function(var)
{
  name <- as.character(substitute(var))
  pbdenv$status[[name]]
}

set.status <- function(var, val)
{
  name <- as.character(substitute(var))
  pbdenv$status[[name]] <- val
  invisible()
}



### ------------------------------------------------
pbd_readline <- function(input)
{
  if (get.status(continuation))
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
    if (grepl(x=input, pattern="^(q\\(|quit\\()", perl=TRUE)) 
      inputs[i] <- "pbd_exit()"
    else if (grepl(x=input, pattern="^(\\?|\\?\\?|help\\()", perl=TRUE))
      stop("not supported") # FIXME make this an internal stop error
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



pbd_repl_printer <- function()
{
  if (pbdenv$whoami == "remote") return(invisible())
  
#  cat("----------------------------\n")
#  print(ret)
#  cat("----------------------------\n")
  
  if (get.status(visible))
    cat(paste(get.status(ret), collapse="\n"), "\n")
  
  if (get.status(num_warnings) > 0)
  {
    if (get.status(num_warnings) > 10)
      cat(paste("There were", get.status(num_warnings), "warnings (use warnings() to see them)\n"))
    else
      print(warnings())
  }
  
  return(invisible())
}



pbd_interrupt <- function(x)
{
#  pbdenv$status$pbd_prompt_active <- TRUE
  cat("interrupt\n")
  print(x)
}



pbd_warning <- function(warn)
{
  pbdenv$status$num_warnings <- pbdenv$status$num_warnings + 1
  
  pbdenv$status$warnings <- append(pbdenv$status$warnings, conditionMessage(warn))
  invokeRestart("muffleWarning")
  print(warn)
}



pbd_error <- function(err)
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



pbd_show_errors <- function()
{
  if (!is.null(get.status(lasterror)))
    cat(get.status(lasterror))
  
  invisible()
}



pbd_show_warnings <- function()
{
  warnings <- get.status(warnings)
  nwarnings <- length(warnings)
  
  if (!is.null(warnings))
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
  
  invisible()
}



pbd_bcast <- function(msg)
{
  if (pbdenv$bcast_method == "mpi")
  {
    msg <- bcast(msg, rank.source=0)
  }
  else if (pbdenv$bcast_method == "zmq")
  {
    if (comm.size() > 1)
    {
      if (comm.rank() == 0)
      {
        for (rnk in 1:(comm.size()-1))
          send.socket(pbdenv$remote_socket, data=msg)
      }
      else
      {
        msg <- receive.socket(pbdenv$remote_socket)
      }
    }
  }
  
  return(msg)
}



pbd_eval <- function(input, whoami, env)
{
  set.status(continuation, FALSE)
  set.status(lasterror, NULL)
  
  if (whoami == "local")
  {
    send.socket(pbdenv$socket, data=input)
    
    ### Special cases
    if (all(grepl(x=input, pattern="^pbd_localize\\(", perl=TRUE)))
      eval(parse(text=input))
    else if (all(grepl(x=input, pattern="^ls.local\\(", perl=TRUE)))
      eval(parse(text=input))
    else if (all(grepl(x=input, pattern="^rm.local\\(", perl=TRUE)))
      eval(parse(text=input))
    else if (all(grepl(x=input, pattern="^eval.local\\(", perl=TRUE)))
      eval(parse(text=input))
    
    pbdenv$status <- receive.socket(pbdenv$socket)
    
    pbd_show_errors()
    pbd_show_warnings()
  }
  else if (whoami == "remote")
  {
    if (comm.rank() == 0)
    {
      cat("Awaiting message:  ")
      msg <- receive.socket(pbdenv$socket)
      cat(msg, "\n")
    }
    else
      msg <- NULL
    
    msg <- pbd_bcast(msg)
    
    barrier() # just in case ...
    
    ret <- 
    withCallingHandlers(
      tryCatch({
          pbdenv$visible <- withVisible(eval(parse(text=msg), envir=env))
        }, interrupt=pbd_interrupt, error=pbd_error
      ), warning=pbd_warning
    )
    
    
    if (comm.rank() == 0)
    {
      if (!is.null(ret))
      {
        set.status(visible, ret$visible)
        
        if (!ret$visible)
          set.status(ret, NULL)
        else
          set.status(ret, capture.output(ret$value))
      }
      
      send.socket(pbdenv$socket, pbdenv$status)
    }
    
  }
  else
    stop("bad 'whoami'")
}



pbd_exit <- function()
{
  set.status(should_exit, TRUE)
  
  return(invisible())
}



pbd_repl_init <- function()
{
  if (!get.status(pbd_prompt_active))
    set.status(pbd_prompt_active, TRUE)
  else
  {
    cat("The pbd repl is already running!\n")
    return(FALSE)
  }
  
  ### Initialize zmq
  if (pbdenv$whoami == "local")
  {
    pbdenv$context <- init.context()
    pbdenv$socket <- init.socket(pbdenv$context, "ZMQ_REQ")
    connect.socket(pbdenv$socket, paste0("tcp://localhost:", pbdenv$port))
  }
  else if (pbdenv$whoami == "remote")
  {
    ### Order very much matters!
    suppressPackageStartupMessages(library(pbdMPI))
    
    if (comm.size() == 1)
      cat("WARNING:  You should restart with mpirun and more than 1 MPI rank.\n")
    
    if (comm.rank() == 0)
      cat("Hello! This is the server; please don't type things here!\n\n")
    
    if (comm.rank() == 0)
    {
      ### client/server
      pbdenv$context <- init.context()
      pbdenv$socket <- init.socket(pbdenv$context, "ZMQ_REP")
      bind.socket(pbdenv$socket, paste0("tcp://*:", pbdenv$port))
    }
    
    if (pbdenv$bcast_method == "zmq")
    {
      if (comm.size() > 1)
      {
        if (comm.rank() == 0)
        {
          ### rank 0 setup for talking to other ranks
          pbdenv$remote_context <- init.context()
          pbdenv$remote_socket <- init.socket(pbdenv$remote_context, "ZMQ_PUSH")
          bind.socket(pbdenv$remote_socket, paste0("tcp://*:", pbdenv$remote_port))
        }
        else
        {
          ### other ranks
          pbdenv$remote_context <- init.context()
          pbdenv$remote_socket <- init.socket(pbdenv$remote_context, "ZMQ_PULL")
          connect.socket(pbdenv$remote_socket, paste0("tcp://localhost:", pbdenv$remote_port))
        }
      }
    }
  }
  
  return(TRUE)
}



pbd_repl <- function(env=sys.parent())
{
  ### FIXME needed?
  if (!interactive() && pbdenv$whoami == "local")
    stop("You should only use this interactively")
  
  init <- pbd_repl_init()
  if (!init) return(invisible())
  
  
  ### the repl
  while (TRUE)
  {
    input <- character(0)
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      pbdenv$visible <- withVisible(invisible())
      input <- pbd_readline(input=input)
      
      pbd_eval(input=input, whoami=pbdenv$whoami, env=env)
      
      if (get.status(continuation)) next
      
      pbd_repl_printer()
      
      ### Should go after all other evals and handlers
      if (get.status(should_exit))
      {
        set.status(pbd_prompt_active, FALSE)
        set.status(should_exit, FALSE)
        return(invisible())
      }
      
      break
    }
  }
  
  set.status(pbd_prompt_active, FALSE)
  set.status(should_exit, FALSE)
  
  return(invisible())
}



pbd_localize <- function(object, newname)
{
  err <- ".__pbd_localize_failure"
  
  if (pbdenv$whoami == "local")
  {
    value <- receive.socket(pbdenv$socket)
    name <- as.character(substitute(object))
    
    if (value == err)
    {
      cat(paste0("Error: object '", name, "' not found\n"))
      return(invisible(FALSE))
    }
    
    if (!missing(newname))
      name <- newname
    
    assign(x=name, value=value, envir=.GlobalEnv)
    
    ret <- TRUE
  }
  else if (pbdenv$whoami == "remote")
  {
    if (comm.rank() == 0)
    {
      if (!exists(deparse(substitute(object))))
        ret <- send.socket(pbdenv$socket, data=err, send.more=TRUE)
      else
        ret <- send.socket(pbdenv$socket, data=object, send.more=TRUE)
    }
  }
  
  return(invisible(ret))
}



ls.local <- function(envir, all.names=FALSE, pattern)
{
  if (missing(envir))
    envir <- .GlobalEnv
  
  if (pbdenv$whoami == "local")
    print(ls(envir=envir, all.names=all.names, pattern=pattern))
  
  return(invisible())
}



### TODO error checking
rm.local <- function(..., list=character(), envir)
{
  if (pbdenv$whoami == "local")
  {
    if (missing(envir))
      envir <- .GlobalEnv
    
    ### FIXME this is disgusting
    objs <- match.call(expand.dots=TRUE)
    objs[[1]] <- NULL
    
    rm(list=as.character(objs), envir=envir)
  }
  
  return(invisible())
}


### TODO basically everything
eval.local <- function(expr)
{
  if (pbdenv$whoami == "local")
    print(eval(expr=expr))
  
  return(invisible)
}


