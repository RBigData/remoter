receive_ <- function()
{
  receive.socket(.pbdenv$socket)
}



send_ <- function(data, send.more=FALSE)
{
  send.socket(.pbdenv$socket, data=data, send.more=send.more)
}



send <- function(data, send.more=FALSE)
{
  serialized <- serialize(data, NULL)
  encrypted <- sodium::auth_encrypt(serialized, getkey(private), getkey(theirs))
  send_(data=encrypted, send.more=send.more)
}



receive <- function()
{
  encrypted <- receive_()
  
  if (identical(encrypted, magicmsg_first_connection))
  {
    first_receive()
    return(magicmsg_first_connection)
  }
  
  raw <- sodium::auth_decrypt(encrypted, getkey(private), getkey(theirs))
  
  unserialize(raw)
}



first_connect <- function()
{
  send_(magicmsg_first_connection)
  .pbdenv$keys$theirs <- receive_()
  send_(getkey(public))
  
  invisible()
}



first_receive <- function()
{
  send_(getkey(public))
  .pbdenv$keys$theirs <- receive_()
  
  invisible()
}
