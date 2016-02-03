#' exit
#' 
#' Exit the remoter client/server.
#' 
#' @description
#' This function cleanly shuts down the remoter server the client
#' is currently connected to, as well as shutting down the client.
#' One can also use \code{q()} (while the client is running), and
#' this will not close the active R session on the client.
#' 
#' 
#' @param client.only
#' Logical; if \code{TRUE}, then the client disconnects from
#' the server.  Otherwise, the server is shut down together 
#' with the client.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
exit <- function(client.only=TRUE)
{
  if (!assert_nostop(is.flag(client.only))) return(invisible(FALSE))
  
  if (!client.only || .pbdenv$whoami == "local")
  {
    set.status(should_exit, TRUE)
    
    if (.pbdenv$whoami == "remote")
      logprint("client killed server")
  }
  else
    logprint("client disconnected with call to exit()")
  
  return(invisible(TRUE))
}



### For internal consistency
remoter_exit <- exit
