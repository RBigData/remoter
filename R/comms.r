### When a client first connects:
# * client sends magicmsg_first_connection
# * server sends value of .pbdenv$secure (TRUE or FALSE)
# * client either responds "" or hangs up if it can't continue
# * key exchange if necessary
# * next operation is to check if server needs a password...

### Comm pattern
# * pattern is req/rep
# * client always goes send/recv (unless send.more=TRUE)
# * server always goes recv/send

send_unsecure <- function(data, send.more=FALSE)
{
  send.socket(.pbdenv$socket, data=data, send.more=send.more)
}



send_secure <- function(data, send.more=FALSE)
{
  serialized <- serialize(data, NULL)
  encrypted <- sodium::auth_encrypt(serialized, getkey(private), getkey(theirs))
  send_unsecure(data=encrypted, send.more=send.more)
}



receive_unsecure <- function()
{
  msg <- receive.socket(.pbdenv$socket)
  
  if (identical(msg, magicmsg_first_connection))
  {
    first_receive()
    return(magicmsg_first_connection)
  }
  
  msg
}



receive_secure <- function()
{
  encrypted <- receive_unsecure()
  
  if (identical(encrypted, magicmsg_first_connection))
  {
    first_receive()
    return(magicmsg_first_connection)
  }
  
  raw <- sodium::auth_decrypt(encrypted, getkey(private), getkey(theirs))
  unserialize(raw)
}



send <- function(data, send.more=FALSE)
{
  if (.pbdenv$secure)
    send_secure(data=data, send.more=send.more)
  else
    send_unsecure(data=data, send.more=send.more)
}



receive <- function()
{
  if (.pbdenv$secure)
    receive_secure()
  else
    receive_unsecure()
}



first_connect <- function()
{
  send_unsecure(magicmsg_first_connection)
  security <- receive_unsecure()
  
  if (security && !has.sodium())
    stop("remoter server communications are encrypted; please install the 'sodium' package, or start an unsecure server.")
  else if (!security && has.sodium())
    cat("WARNING: server not secure; communications are not encrypted.")
  
  .pbdenv$secure <- security
  
  if (.pbdenv$secure)
  {
    send_unsecure(NULL)
    .pbdenv$keys$theirs <- receive_unsecure()
    send_unsecure(getkey(public))
  }
  else
    send_unsecure(NULL)
  
  invisible()
}



first_receive <- function()
{
  send_unsecure(.pbdenv$secure)
  
  if (.pbdenv$secure)
  {
    receive_unsecure()
    send_unsecure(getkey(public))
    .pbdenv$keys$theirs <- receive_unsecure()
  }
  else
    receive_unsecure()
  
  invisible()
}
