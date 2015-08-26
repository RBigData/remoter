### For R CMD check
utils::globalVariables(c("continuation", "lasterror", "visible", "should_exit", "pbd_prompt_active", "ret", "lasterror"))


#' State management for the pbdR Client/Server
#' 
#' @export
pbdenv <- new.env()

# options
pbdenv$prompt <- "pbdR"
pbdenv$port <- 55555
pbdenv$remote_port <- 55556
pbdenv$bcast_method <- "zmq"


# internals
pbdenv$context <- NULL
pbdenv$socket <- NULL
pbdenv$debug <- FALSE
pbdenv$verbose <- TRUE
pbdenv$client_lasterror <- ""

pbdenv$remote_context <- NULL
pbdenv$remote_socket <- NULL

# C/S state
pbdenv$status <- list(
  ret               = invisible(),
  visible           = FALSE,
  lasterror         = NULL,
  shouldwarn        = FALSE,
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



pbd_client_stop <- function(msg)
{
  pbdenv$client_lasterror <- msg
  cat("Error: ", msg, "\n")
  
  invisible()
}



pbd_sanitize <- function(inputs)
{
  for (i in 1:length(inputs))
  {
    input <- inputs[i]
    if (grepl(x=input, pattern="^(q\\(|quit\\()", perl=TRUE)) 
      inputs[i] <- "pbd_exit()"
    else if (grepl(x=input, pattern="^geterrmessage\\(", perl=TRUE))
      inputs[i] <- pbdenv$client_lasterror
    else if (grepl(x=input, pattern="^(\\?|\\?\\?|help\\()", perl=TRUE))
    {
      pbd_client_stop("Reading help files from the server is currently not supported.")
      inputs[i] <- "invisible()"
    }
    else if (grepl(x=input, pattern="^warnings\\(", perl=TRUE))
    {
      pbdenv$status$shouldwarn <- TRUE
      pbd_show_warnings()
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



pbd_repl_printer <- function()
{
  if (pbdenv$whoami == "remote")
  {
    pbdenv$status$shouldwarn <- FALSE
    return(invisible())
  }
  
  if (get.status(visible))
    cat(paste(get.status(ret), collapse="\n"), "\n")
  
  pbd_show_errors()
  pbd_show_warnings()
  
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
  pbdenv$status$shouldwarn <- TRUE
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
  
  if (!is.null(warnings) && pbdenv$status$shouldwarn)
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
  
  pbdenv$status$shouldwarn <- FALSE
  
  invisible()
}



# pbd_bcast_mpi <- function(msg) bcast(msg, rank.source=0)
pbd_bcast_mpi <- function(msg) spmd.bcast.message(msg, rank.source = 0)

pbd_bcast_zmq <- function(msg)
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
  
  msg
}

pbd_bcast <- function(msg)
{
  if (pbdenv$bcast_method == "mpi")
    msg <- pbd_bcast_mpi(msg=msg)
  else if (pbdenv$bcast_method == "zmq")
    msg <- pbd_bcast_zmq(msg=msg)
  
  return(msg)
}



### TODO FIXME integrate with pbd_sanitize()
pbd_eval_filter_server <- function(msg)
{
  if (all(grepl(x=msg, pattern="^library\\(", perl=TRUE)))
  {
    if (comm.rank() == 0)
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
    else
      msg <- paste0("suppressPackageStartupMessages(", msg, ")")
  }
  
  msg
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
    
#    pbd_show_errors()
#    pbd_show_warnings()
  }
  else if (whoami == "remote")
  {
    if (comm.rank() == 0)
    {
      if (pbdenv$debug)
        cat("Awaiting message:  ")
      
      msg <- receive.socket(pbdenv$socket)
      
      if (pbdenv$debug)
        cat(msg, "\n")
    }
    else
      msg <- NULL
    
    msg <- pbd_bcast(msg)
    barrier() # just in case ...
    
    msg <- pbd_eval_filter_server(msg=msg)
    
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
          set.status(ret, utils::capture.output(ret$value))
      }
      
      send.socket(pbdenv$socket, pbdenv$status)
    }
    
  }
  else
    stop("bad 'whoami'")
}



#' pbd_exit
#' 
#' Exit the pbdR client/server.
#' 
#' @description
#' This function cleanly shuts down any pbdR servers which have been
#' spawned, as well as shutting down the client.  Failing to use
#' this function to shut down the client/server may cause unexpected
#' results.
#' 
#' @export
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
    
    cat("please wait a moment for the servers to spin up...")
    pbd_eval(input="barrier()", whoami=pbdenv$whoami)
    cat("\n")
  }
  else if (pbdenv$whoami == "remote")
  {
    ### Order very much matters!
###    suppressPackageStartupMessages(library(pbdMPI))
    
    if (pbdenv$debug)
    {
      if (comm.size() == 1)
        cat("WARNING:  You should restart with mpirun and more than 1 MPI rank.\n")
      
      if (comm.rank() == 0)
        cat("Hello! This is the server; please don't type things here!\n\n")
    }
    
    if (comm.rank() == 0)
    {
      serverip <- getip()
      invisible(bcast(serverip, rank.source=0))
      
      ### client/server
      pbdenv$context <- init.context()
      pbdenv$socket <- init.socket(pbdenv$context, "ZMQ_REP")
      bind.socket(pbdenv$socket, paste0("tcp://*:", pbdenv$port))
    }
    else
      serverip <- bcast()
    
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
          connect.socket(pbdenv$remote_socket, paste0("tcp://", serverip, ":", pbdenv$remote_port))
        }
      }
    }
  }
  
  
  
  return(TRUE)
}



#' pbd_repl
#' 
#' The REPL for the client/server.
#' 
#' @description
#' This is exported for clean access reasons; you shoud not directly
#' use this function.
#' 
#' @param env 
#' Environment where repl evaluations will take place.
#'
#' @export
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



#' pbd_localize
#' 
#' Localize R objects.
#' 
#' @description
#' This function allows you to pass an object from MPI rank 0 of 
#' the servers to the local R session behind the client.
#' 
#' @param object 
#' A remote R object.
#' @param newname
#' The name the object should take when it becomes local. If left blank,
#' the local name will have the original (remote) object's name.
#' @param env
#' The environment into which the assignment will take place. The
#' default is the global environment.
#' 
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs remotely
#' > library(pbdCS)
#' > y
#' ###  Error: object 'y' not found
#' > pbd_launch_servers()
#' > pbd_launch_client()
#' pbdR> x
#' ### Error: object 'x' not found
#' pbdR> x <- "some data"
#' pbdR> x
#' ###  [1] "some data" 
#' pbdR> pbd_localize(x, "y")
#' pbdR> pbd_exit()
#' > y
#' ###  [1] "some data"
#' }
#' 
#' @export
pbd_localize <- function(object, newname, env=.GlobalEnv)
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
    
    assign(x=name, value=value, envir=env)
    
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



#' ls.local
#' 
#' View objects on the client.
#' 
#' @description
#' A function to view environments on the client's R session.  To
#' view objects on the server, use \code{ls()}.
#' 
#' @param envir
#' Environment (as in \code{ls()}).
#' @param all.names
#' Logical that determines if all names are returned or those beginning
#' with a '.' are omitted (as in \code{ls()}).
#' @param pattern
#' Optional regular expression (as in \code{ls()}).
#'
#' @export
ls.local <- function(envir, all.names=FALSE, pattern)
{
  if (missing(envir))
    envir <- .GlobalEnv
  
  if (pbdenv$whoami == "local")
    print(ls(envir=envir, all.names=all.names, pattern=pattern))
  
  return(invisible())
}



#' rm.local
#' 
#' View objects on the client.
#' 
#' @description
#' A function to remove objects from the client's R session.  To
#' remove objects on the server, use \code{rm()}.
#' 
#' @param ...
#' Objects to be removed from the client's R session.
#' @param list
#' Character vector naming objects to be removed (as in \code{rm()}).
#' @param envir
#' Environment (as in \code{rm()}).
#'
#' @export
rm.local <- function(..., list=character(), envir)
{
  ### TODO error checking
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



#' eval.local
#' 
#' Evaluate expressions on the client.
#' 
#' @description
#' A function to evaluate expressions on the client's R session.
#' 
#' @param expr
#' Expression to be evaluated on the client.
#'
#' @export
eval.local <- function(expr)
{
  ### TODO basically everything
  if (pbdenv$whoami == "local")
    print(eval(expr=expr))
  
  return(invisible)
}


