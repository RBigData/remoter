receive <- function()
{
  receive.socket(.pbdenv$socket)
}



send <- function(data, send.more=FALSE)
{
  send.socket(.pbdenv$socket, data=data, send.more=send.more)
}



receive_crypt <- function()
{
  
}



send_crypt <- function()
{
  
}
