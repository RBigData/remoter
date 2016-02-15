remoter_read_password <- function()
{
  pw <- readline("PASSWORD:  ") 
  
  pw
}



remoter_check_password_local <- function()
{
  first_send()
  needpw <- receive()
  
  while (needpw)
  {
    pw <- remoter_read_password()
    send(pw)
    check <- receive()
    
    if (isTRUE(check))
      break
    else if (is.null(check))
      stop("Max attempts reached; killing server...")
    
    cat("Sorry, try again.\n")
  }
}



remoter_check_password_remote <- function()
{
  if (is.null(getval(password)))
  {
    logprint(level="PASS", "alerting client no password required", checkverbose=TRUE)
    send(FALSE)
  }
  else
  {
    logprint("client attempting to connect...")
    logprint(level="PASS", "alerting client a password is required", checkverbose=TRUE)
    send(TRUE)
    
    attempts <- 2L
    while (TRUE)
    {
      logprint(level="PASS", "receiving password attempt", checkverbose=TRUE)
      pw <- receive()
      if (pw == getval(password))
      {
        logprint("client password authenticated")
        send(TRUE)
        break
      }
      else if (attempts <= getval(maxattempts))
      {
        logprint(level="PASS", "received bad password", checkverbose=TRUE)
        send(FALSE)
      }
      else
      {
        logprint(level="PASS", "alert client max password attempts reached", checkverbose=TRUE)
        send(NULL)
        logprint(paste0("received maxretry=", getval(maxattempts), " bad passwords; terminating self..."))
        if (getval(kill_interactive_server))
          q("no")
        else
          stop("Max password attempts reached.")
      }
      
      attempts <- attempts + 1L
    }
  }
}



remoter_check_version_local <- function()
{
  send(get_versions())
  check <- receive()
  
  if (!check)
    stop("Incompatible package versions; quitting client (perhaps you need to update and restart the server?)")
  
  invisible(TRUE)
}



remoter_check_version_remote <- function()
{
  logprint("VERS: checking client package versions", checkverbose=TRUE)
  versions_client <- receive()
  versions_server <- get_versions()
  check <- compare_versions(versions_client, versions_server)
  
  logprint(level="VERS", "send version check result to client", checkverbose=TRUE)
  send(check)
  
  if (check)
  {
    logprint(level="VERS", "client version passes version check", checkverbose=TRUE)
    logprint("client connected")
  }
  else
    logprint("client/server version mismatch; client kicked.")
  
  invisible(TRUE)
}
