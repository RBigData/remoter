remoter_read_password <- function()
{
  # tt <- tktoplevel() 
  # pw <- tclVar("") 
  # 
  # label <- tklabel(tt, text="PASSWORD") 
  # textbox <- tkentry(tt, show="*", textvariable=pw) 
  # tkbind(textbox, "<Return>", function() tkdestroy(tt)) 
  # button <- tkbutton(tt,text="ok", default="active", command=function() tkdestroy(tt)) 
  # tkpack(label, textbox, button) 
  # 
  # tkwait.window(tt) 
  # 
  # return(tclvalue(pw)) 
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
  if (is.null(.pbdenv$password))
  {
    logprint("PW: alerting client no password required", checkverbose=TRUE)
    send(FALSE)
    logprint("client connected")
  }
  else
  {
    logprint("client attempting to connect...")
    logprint("PW: alerting client a password is required", checkverbose=TRUE)
    send(TRUE)
    
    attempts <- 2L
    while (TRUE)
    {
      logprint("PW: receiving password attempt", checkverbose=TRUE)
      pw <- receive()
      if (pw == .pbdenv$password)
      {
        logprint("client connected")
        send(TRUE)
        break
      }
      else if (attempts <= .pbdenv$maxattempts)
        send(FALSE)
      else
      {
        logprint("PW: alert client max password attempts reached", checkverbose=TRUE)
        send(NULL)
        logprint(paste0("received maxretry=", .pbdenv$maxattempts, " bad passwords; terminating self..."))
        stop("Max password attempts reached.")
      }
      
      attempts <- attempts + 1L
    }
  }
}



remoter_check_version_local <- function()
{
  send(NULL)
  versions_server <- receive()
  
  if (!isTRUE(versions_server))
  {
    versions_client <- get_versions()
    if (!compare_versions(versions_client, versions_server))
      stop("Incompatible package versions; quitting client (perhaps you need to update and restart the server?)")
  }
  
  invisible(TRUE)
}



remoter_check_version_remote <- function()
{
  receive()
  
  if (!.pbdenv$checkversion)
  {
    logprint("VER: alerting client no version checking should occur", checkverbose=TRUE)
    send(FALSE)
  }
  else
  {
    versions <- get_versions()
    logprint("VER: sending package versions to client", checkverbose=TRUE)
    send(versions)
  }
  
  invisible(TRUE)
}
