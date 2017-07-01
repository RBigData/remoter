#' Batch Execution
#' 
#' Run a script on a remote server in batch.  Similar to R's own
#' \code{source()} function.
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
#' @param file
#' A character string pointing to the file you wish to execute/source. Either
#' this or \code{script} (but not both) should be procided.
#' @param script
#' A character string containing the commands you wish to execute/source. Either
#' this or \code{script} (but not both) should be procided.
#' @param timer
#' Logical; should the "performance prompt", which shows timing
#' statistics after every command, be used?
#' 
#' @examples
#' \dontrun{
#' # Run a script in an R file
#' file <- "/path/to/an/R/script.r"
#' batch(file=file)
#' 
#' # Run a script stored in a character vector
#' script <- "1+1"
#' batch(script=script)
#' }
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
batch <- function(addr="localhost", port=55555, password=NULL, file, script,
  timer=FALSE)
{
  check.is.flag(timer)
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(port, warn=FALSE)
  
  if (missing(file) && missing(script))
    stop("At least one of the arguments 'script' or 'file' should be provided")
  else if (missing(file))
  {
    check.is.string(script)
    src <- unlist(strsplit(script, split="\n"))
  }
  else if (missing(script))
  {
    check.is.string(file)
    check(file.exists(file))
    src <- readLines(file)
  }
  else
    stop("Only one of the arguments 'script' or 'file' should be provided")
  
  test_connection(addr, port)
  
  reset_state()
  
  set(whoami, "local")
  set(timer, timer)
  set(port, port)
  set(remote_addr, addr)
  set(clientpw, password)
  
  set(isbatch, TRUE)
  
  remoter_repl_batch(src=src)
  
  invisible(TRUE)
}



remoter_repl_batch <- function(src, env=globalenv())
{
  test <- remoter_init_client()
  if (!test) return(FALSE)
  
  timer <- getval(timer)
  EVALFUN <- timerfun(timer)
  
  len <- length(src)
  line <- 1L
  
  while (TRUE)
  {
    input <- character(0)
    set.status(continuation, FALSE)
    set.status(visible, FALSE)
    
    while (TRUE)
    {
      tmp <- src[line]
      
      if (gsub(tmp, pattern=" +", replacement="") == "")
      {
        line <- line + 1L
        next
      }
      
      input <- c(input, src[line])
      
      timing <- EVALFUN({
        remoter_client_sendrecv(input=input, env=env)
      })
      
      if (get.status(continuation))
      {
        line <- line + 1L
        next
      }
      
      if (timer)
      {
        cat("## ")
        cat(input)
        cat("\n")
      }
      
      remoter_repl_printer()
      
      timerprint(timer, timing)
      
      break
    }
    
    line <- line + 1L
    
    if (line > len)
      break
  }
  
  set.status(prompt_active, FALSE)
  set.status(should_exit, FALSE)
  
  return(invisible())
}
