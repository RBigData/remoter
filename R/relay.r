#' Relay Launcher
#' 
#' Launcher for the remoter relay.
#' 
#' @details
#' The relay is an intermediary or "middleman" between the client
#' and server meant for machines with split login/compute nodes.
#' 
#' @param addr
#' The address of the server.
#' @param recvport
#' The port for receiving commands from the client.
#' @param sendport
#' The port for sending commands to the server.
#' @param verbose
#' Show verbose messaging.
#' 
#' @return
#' Returns \code{TRUE} invisibly on successful exit.
#' 
#' @export
relay <- function(addr, recvport=55556, sendport=55555, verbose=FALSE)
{
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(recvport)
  validate_port(sendport)
  assert_that(recvport != sendport)
  assert_that(is.flag(verbose))
  
  reset_state()
  
  set(whoami, "relay")
  set(remote_addr, addr)
  set(recvport, recvport)
  set(sendport, sendport)
  set(verbose, verbose)
  
  logprint(paste("*** Launching relay ***"), preprint="\n")
  
  remoter_repl_relay()
  remoter_exit_server()
  
  invisible(TRUE)
}



remoter_repl_relay <- function()
{
  ### client/relay comms
  ctxt.recv <- pbdZMQ::init.context()
  socket.recv <- pbdZMQ::init.socket(ctxt.recv, "ZMQ_REP")
  addr <- pbdZMQ::address("*", getval(recvport))
  pbdZMQ::bind.socket(socket.recv, addr)
  
  ### relay/server comms
  ctxt.send <- pbdZMQ::init.context()
  socket.send <- pbdZMQ::init.socket(ctxt.send, "ZMQ_REQ")
  addr <- pbdZMQ::address(getval(remote_addr), getval(sendport))
  pbdZMQ::connect.socket(socket.send, addr)
  
  while (TRUE)
  {
    ### receive from client, send to server
    data <- pbdZMQ::receive.socket(socket=socket.recv, unserialize=FALSE)
    logprint("Received message from client. Sending to server.", checkverbose=TRUE)
    pbdZMQ::send.socket(socket=socket.send, data=data, serialize=FALSE)
    
    ### receive from server, send to client
    data <- pbdZMQ::receive.socket(socket=socket.send, unserialize=FALSE)
    logprint("Received response from server. Sending to client.", checkverbose=TRUE)
    pbdZMQ::send.socket(socket=socket.recv, data=data, serialize=FALSE)
  }
  
  return(invisible())
}
