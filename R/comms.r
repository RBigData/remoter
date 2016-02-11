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
  encrypted <- receive.socket(.pbdenv$socket)
  
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



first_send <- function()
{
  send_unsecure(magicmsg_first_connection)
  security <- receive_unsecure()
  
  if (security && !has.sodium())
    stop("remoter server communications are encrypted but the 'sodium' package is not detected on the client.  Please install the 'sodium' package, or start an unsecure server.")
  else if (!security && has.sodium())
    cat("WARNING: server not secure; communications are not encrypted.\n")
  
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
  logprint("Receiving first connection from client...", checkverbose=TRUE)
  logprint(paste("INIT: alerting that server", ifelse(.pbdenv$secure, "is", "isn't"), "secure"), checkverbose=TRUE)
  send_unsecure(.pbdenv$secure)
  
  logprint("INIT: receiving security acknowledgement from client", checkverbose=TRUE)
  if (.pbdenv$secure)
  {
    receive_unsecure()
    logprint("AUTH: sending server public key", checkverbose=TRUE)
    send_unsecure(getkey(public))
    logprint("AUTH: receiving client public key", checkverbose=TRUE)
    .pbdenv$keys$theirs <- receive_unsecure()
  }
  else
    receive_unsecure()
  
  invisible()
}
