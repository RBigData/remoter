#' ls on Client
#' 
#' View objects on the client.
#' 
#' @description
#' A function to view environments on the client's R session.  To
#' view objects on the server, just use \code{ls()}.  Instead of
#' using this function, you could also just kill the client, do your
#' local operations, then re-run your \code{client()} command.
#' 
#' @param envir
#' Environment (as in \code{ls()}).
#' @param all.names
#' Logical that determines if all names are returned or those beginning
#' with a '.' are omitted (as in \code{ls()}).
#' @param pattern
#' Optional regular expression (as in \code{ls()}).
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#'
#' @export
lsc <- function(envir, all.names=FALSE, pattern)
{
  if (missing(envir))
    envir <- .GlobalEnv
  
  if (iam("local"))
    print(ls(envir=envir, all.names=all.names, pattern=pattern))
  
  return(invisible(TRUE))
}



#' rmc
#' 
#' Remove objects on the client.
#' 
#' @description
#' A function to remove objects from the client's R session.  To
#' remove objects on the server, just use \code{rm()}.  Instead of
#' using this function, you could also just kill the client, do your
#' local operations, then re-run your \code{client()} command.
#' 
#' @param ...
#' Objects to be removed from the client's R session.
#' @param list
#' Character vector naming objects to be removed (as in \code{rm()}).
#' @param envir
#' Environment (as in \code{rm()}).
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#'
#' @export
rmc <- function(..., list=character(), envir)
{
  ### TODO error checking
  if (iam("local"))
  {
    if (missing(envir))
      envir <- .GlobalEnv
    
    ### FIXME this is disgusting
    objs <- match.call(expand.dots=TRUE)
    objs[[1]] <- NULL
    
    rm(list=as.character(objs), envir=envir)
  }
  
  return(invisible(TRUE))
}



#' evalc
#' 
#' Evaluate expressions on the client.
#' 
#' @description
#' A function to evaluate expressions on the client's R session.  To
#' eval expressions on the server, just use \code{eval()}.  Instead of
#' using this function, you could also just kill the client, do your
#' local operations, then re-run your \code{client()} command.
#' 
#' @param expr
#' Expression to be evaluated on the client.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#'
#' @export
evalc <- function(expr)
{
  ### TODO basically everything
  if (iam("local"))
    print(eval(expr=expr))
  
  return(invisible(TRUE))
}
