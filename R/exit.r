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
#' @param q.server
#' Logical; if \code{TRUE}, then the server calls \code{q("no")}
#' after shuting down with the client.  This is useful for cases
#' where the server is running in an interactive R session, and you
#' wish to shut the entire thing down.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
exit <- function(client.only=TRUE, q.server=TRUE)
{
  if (!assert_nostop(is.flag(client.only))) 
    return(invisible(FALSE))
  if (!assert_nostop(is.flag(q.server)))
    return(invisible(FALSE))
  
  if (!client.only || iam("local"))
  {
    set.status(should_exit, TRUE)
    
    if (iam("remote"))
      logprint("client killed server")
  }
  else
    logprint("client disconnected with call to exit()")
  
  if(!client.only && q.server)
  {
    if (iam("remote") && interactive())
      set(kill_interactive_server, TRUE)
  }

  return(invisible(TRUE))
}



### For internal consistency
remoter_exit <- exit
