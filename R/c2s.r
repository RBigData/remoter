#' Client-to-Server Object Transfer
#' 
#' Localize R objects.
#' 
#' @description
#' This function allows you to pass an object from the local R 
#' session (the client) to server.
#' 
#' @param object 
#' A local R object.
#' @param newname
#' The name the object should take when it is stored on the remote
#' server. If left blank, the remote name will be the same as the
#' original (local) object's name.
#' @param env
#' The environment into which the assignment will take place. The
#' default is the remoter "working environment".
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs remotely
#' > library(remoter)
#' > x <- "some data"
#' > remoter::connect("my.remote.server")
#' remoteR> x
#' ### Error: object 'x' not found
#' remoteR> c2s(x)
#' remoteR> x
#' ###  [1] "some data" 
#' }
#' 
#' @export
c2s <- function(object, newname, env)
{
  err <- ".__remoter_c2s_failure"
  name <- as.character(substitute(object))
  
  if (iam("local"))
  {
    receive()
    
    value <- get0(name, ifnotfound=err)
    send(data=value)
    
    if (identical(value, err))
    {
      cat(paste0("Error: object '", name, "' not found on the client\n"))
      return(invisible(FALSE))
    }
  }
  else if (iam("remote"))
  {
    send("")
    
    value <- receive()
    
    if (identical(value, err))
    {
      send(FALSE)
      return(invisible(FALSE))
    }
    
    if (!missing(newname))
      name <- newname
    
    if (missing(env))
      env <- sys.frame(-1)
    
    assign(x=name, value=value, envir=env)
  }
  
  return(invisible(TRUE))
}
