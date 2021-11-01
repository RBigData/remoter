logprint <- function(msg, checkverbose=FALSE, checkshowmsg=FALSE, preprint="", level="", timestamp=TRUE, forcemsg=FALSE)
{
  if (identical(msg, magicmsg_first_connection))
    return(invisible())
  
  if (forcemsg || (getval(serverlog) && !checkverbose && !checkshowmsg) || (getval(verbose) && checkverbose) || (getval(showmsg) && checkshowmsg))
  {
    if (timestamp)
      ts <- paste0("[", Sys.time(), "]: ")
    else
      ts <- ""
    
    logmsg <- paste0(preprint, ts, level, ifelse(level=="", "", ": "), msg, "\n")
    # cat(logmsg)
    message(logmsg, appendLF=FALSE)
    
    if (!forcemsg)
      logprint_file(logmsg)
  }
  
  invisible()
}



logfile_init <- function()
{
  if (getval(serverlog))
  {
    logfile = getval(logfile)
    append = !file.exists(logfile)
    cat("", file=logfile, append=append)
    
    logfile
  }
  else
    NULL
}



logprint_file <- function(logmsg)
{
  if (getval(serverlog))
  {
    cat(logmsg, file=getval(logfile), append=TRUE)
    utils::flush.console()
  }
  
  invisible()
}



#' showlog
#' 
#' Show the server log on the client.
#' 
#' @export
showlog <- function()
{
  if (getval(serverlog) && file.exists(getval(logfile)))
    log = readLines(getval(logfile))
  else
    stop("no log file found!")
  
  c(
    paste("### logfile:", getval(logfile)),
    log
  )
}
