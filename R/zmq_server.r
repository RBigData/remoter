pbd.server <- function(port=5555)
{
  context <- init.context()
  socket <- init.socket(context,"ZMQ_REP")
  bind.socket(socket, paste0("tcp://*:", port))
  
  while(1)
  {
    msg <- receive.socket(socket);
    
    print(msg)
    ans <- eval(parse(text=msg), envir=.GlobalEnv)
    
    ret <- capture.output(print(ans))
    send.socket(socket, ret);
  }
  
  invisible()
}


remote.exec <- function(socket, expr, literal=FALSE)
{
  send.socket(socket, data=expr)
  receive.socket(socket)
}


pbd.client <- function(port=5555)
{
  substitute(expr)
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket, paste0("tcp://localhost:", port))
  
  return(socket)
}


