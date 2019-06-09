#' sendfile
#' 
#' Transfer file from client to server.
#' 
#' @param file_send,file_recv
#' The file paths (as strings) for the input/sent file and the output/received
#' file.
#' @param verbose
#' Should file transfer information be printed?
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
sendfile = function(file_send, file_recv, verbose=TRUE)
{
  if (missing(file_send) || !is.string(file_send) || missing(file_recv) || !is.string(file_recv))
  {
    if (iam("local"))
      remoter_client_stop("'file_send' and 'file_recv' must each be a single string")
    
    return(invisible())
  }
  
  if (iam("local"))
  {
    remoter_receive()
    exists = file.exists(file_send)
    remoter_send(exists)
  }
  else if (iam("remote"))
  {
    remoter_send(NULL)
    exists = remoter_receive()
  }
  
  if (!exists)
  {
    if (iam("local"))
      remoter_client_stop("'file_send' does not appear to exist")
    
    return(invisible())
  }
  
  
  socket = getval(socket)
  
  if (iam("local"))
  {
    remoter_receive()
    pbdZMQ::zmq.sendfile(file=file_send, socket=socket, verbose=verbose)
    remoter_send(NULL)
  }
  else if (iam("remote"))
  {
    remoter_send(NULL)
    pbdZMQ::zmq.recvfile(file=file_recv, socket=socket)
    remoter_receive()
  }
  
  invisible(TRUE)
}
