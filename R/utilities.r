#' remoter_localize
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
#' > remoter_launch_servers()
#' > remoter_launch_client()
#' pbdR> x
#' ### Error: object 'x' not found
#' pbdR> x <- "some data"
#' pbdR> x
#' ###  [1] "some data" 
#' pbdR> remoter_localize(x, "y")
#' pbdR> remoter_exit()
#' > y
#' ###  [1] "some data"
#' }
#' 
#' @export
remoter_localize <- function(object, newname, env=.GlobalEnv)
{
  err <- ".__remoter_localize_failure"
  
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
    if (!exists(deparse(substitute(object))))
      ret <- send.socket(pbdenv$socket, data=err, send.more=TRUE)
    else
      ret <- send.socket(pbdenv$socket, data=object, send.more=TRUE)
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


