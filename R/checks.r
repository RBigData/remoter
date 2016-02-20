remoter_check_password_local <- function()
{
  first_send()
  needpw <- remoter_receive()
  
  while (needpw)
  {
    pw <- getPass::getPass()
    if (is.null(pw)) # C-c
    {
      remoter_send(NULL)
      remoter_receive()
      return(FALSE)
    }
    
    remoter_send(pw)
    check <- remoter_receive()
    
    if (isTRUE(check))
      break
    else if (is.null(check))
      stop("Max attempts reached; killing server...")
    
    cat("Sorry, try again.\n")
  }
  
  TRUE
}



remoter_check_password_remote <- function()
{
  if (is.null(getval(password)))
  {
    logprint(level="PASS", "alerting client no password required", checkverbose=TRUE)
    remoter_send(FALSE)
  }
  else
  {
    logprint("client attempting to connect...")
    logprint(level="PASS", "alerting client a password is required", checkverbose=TRUE)
    remoter_send(TRUE)
    
    attempts <- 2L
    while (TRUE)
    {
      logprint(level="PASS", "receiving password attempt", checkverbose=TRUE)
      pw <- remoter_receive()
      if (is.null(pw))
      {
        logprint(level="PASS", "client disconnected", checkverbose=TRUE)
        remoter_send(NULL)
        return(FALSE)
      }
      else if (pw == getval(password))
      {
        logprint("client password authenticated")
        remoter_send(TRUE)
        break
      }
      else if (attempts <= getval(maxattempts))
      {
        logprint(level="PASS", "received bad password", checkverbose=TRUE)
        remoter_send(FALSE)
      }
      else
      {
        logprint(level="PASS", "alert client max password attempts reached", checkverbose=TRUE)
        remoter_send(NULL)
        logprint(paste0("received maxretry=", getval(maxattempts), " bad passwords; terminating self..."))
        if (getval(kill_interactive_server))
          q("no")
        else
          stop("Max password attempts reached.")
      }
      
      attempts <- attempts + 1L
    }
  }
  
  TRUE
}



remoter_check_version_local <- function()
{
  remoter_send(get_versions())
  check <- remoter_receive()
  
  if (!check)
    stop("Incompatible package versions; quitting client (perhaps you need to update and restart the server?)")
  
  invisible(TRUE)
}



remoter_check_version_remote <- function()
{
  logprint("VERS: checking client package versions", checkverbose=TRUE)
  versions_client <- remoter_receive()
  versions_server <- get_versions()
  check <- compare_versions(versions_client, versions_server)
  
  logprint(level="VERS", "send version check result to client", checkverbose=TRUE)
  remoter_send(check)
  
  if (check)
  {
    logprint(level="VERS", "client version passes version check", checkverbose=TRUE)
    logprint("client connected")
  }
  else
    logprint("client/server version mismatch; client kicked.")
  
  invisible(TRUE)
}
