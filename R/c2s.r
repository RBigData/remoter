#' Client-to-Server Transfers
#' 
#' Localize R objects or files.
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
#' @param file_client,file_server
#' Strings. The local file (\code{file_client}) and the path for the remote
#' @param verbose
#' Should file transfer information be printed?
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
#' remoter> x
#' ### Error: object 'x' not found
#' remoter> c2s(x)
#' remoter> x
#' ###  [1] "some data" 
#' }
#' 
#' @rdname c2s
NULL



#' @rdname c2s
#' @export
c2s <- function(object, newname, env=.GlobalEnv)
{
  if (missing(object))
  {
    if (iam("local"))
      remoter_client_stop("must pass an object")
    
    return(invisible())
  }
  
  test <- tryCatch(is.environment(env), error=identity)
  if (isFALSE(test) || inherits(test, "error"))
  {
    if (iam("local"))
    {
      if (isFALSE(test))
        remoter_client_stop("invalid environment")
      else
        remoter_client_stop(gsub(test, pattern="(.*: |\\n)", replacement=""))
    }
    
    return(invisible())
  }
  
  if (!missing(newname))
  {
    if (!identical(make.names(newname), newname))
    {
      if (iam("local"))
        remoter_client_stop("invalid 'newname'")
      
      return(invisible())
    }
  }
  
  
  name <- as.character(substitute(object))
  err <- ".__remoter_s2c_failure"
  
  if (iam("local"))
  {
    remoter_receive()
    
    value <- get0(name, ifnotfound=err)
    remoter_send(data=value)
    
    if (identical(value, err))
    {
      cat(paste0("Error: object '", name, "' not found on the client\n"))
      return(invisible(FALSE))
    }
  }
  else if (iam("remote"))
  {
    remoter_send(NULL)
    
    value <- remoter_receive()
    
    if (identical(value, err))
    {
      remoter_send(FALSE)
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



#' @rdname c2s
#' @export
fc2s <- function(file_client, file_server, verbose=TRUE)
{
  if (missing(file_client))
  {
    if (iam("local"))
      remoter_client_stop("must pass a client file")
    
    return(invisible())
  }
  
  if (missing(file_server))
  {
    if (iam("local"))
      remoter_client_stop("must pass a server file")
    
    return(invisible())
  }
  
  
  port = getval(port)
  ctx = getval(context)
  
  if (iam("local"))
  {
    if (!file.exists(file_client))
      file_state = TRANSFER_MISSING
    else if (is_dir(file_client))
      file_state = TRANSFER_DIR
    else
      file_state = TRANSFER_FILE
    
    remoter_receive()
    remoter_send(file_state)
    
    if (file_state == TRANSFER_MISSING)
    {
      remoter_client_stop("'file_client' does not appear to exist")
      return(invisible())
    }
    
    ### FIXME
    client_ip = "localhost"
    # client_ip = getip::getip("external")
    remoter_receive()
    remoter_send(client_ip)
  }
  else
  {
    remoter_send(NULL)
    file_state = remoter_receive()
    
    if (file_state == TRANSFER_MISSING)
      return(invisible())
    
    remoter_send(NULL)
    client_ip = remoter_receive()
    ### FIXME
    # if (identical(client_ip, getip::getip("external")))
    #   client_ip = "localhost"
  }
  
  
  if (iam("local"))
  {
    rm("socket", envir=.pbdenv)
    set(socket, NULL)
    cleanup()
    
    if (file_state == TRANSFER_DIR)
      pbdZMQ::zmq.senddir(port, file_client, ctx=ctx, verbose=verbose)
    else #if (file_state == TRANSFER_FILE)
      pbdZMQ::zmq.sendfile(port, file_client, ctx=ctx, verbose=verbose)
    
    cleanup()
    
    set(socket, pbdZMQ::init.socket(ctx, "ZMQ_REQ"))
    addr = pbdZMQ::address(getval(remote_addr), port)
    pbdZMQ::connect.socket(getval(socket), addr)
    remoter_send(NULL)
  }
  else if (iam("remote"))
  {
    pbdZMQ::zmq.close(getval(socket))
    
    if (file_state == TRANSFER_DIR)
      pbdZMQ::zmq.recvdir(port, client_ip, file_server, ctx=ctx, verbose=FALSE)
    else #if (file_state == TRANSFER_FILE)
      pbdZMQ::zmq.recvfile(port, client_ip, file_server, ctx=ctx, verbose=FALSE)
    
    cleanup()
    
    set(socket, pbdZMQ::init.socket(ctx, "ZMQ_REP"))
    pbdZMQ::bind.socket(getval(socket), pbdZMQ::address("*", port))
    remoter_receive()
  }
  
  invisible(TRUE)
}
