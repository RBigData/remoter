logfile = tools::file_path_as_absolute(system.file("log/remoterserverlog", package="remoter"))



logprint <- function(msg, checkverbose=FALSE, checkshowmsg=FALSE, preprint="", level="", timestamp=TRUE)
{
  if (identical(msg, magicmsg_first_connection))
    return(invisible())
  
  if ((getval(serverlog) && !checkverbose && !checkshowmsg) || (getval(verbose) && checkverbose) || (getval(showmsg) && checkshowmsg))
  {
    if (timestamp)
      ts <- paste0("[", Sys.time(), "]: ")
    else
      ts <- ""
    
    logmsg <- paste0(preprint, ts, level, ifelse(level=="", "", ": "), msg, "\n")
    # cat(logmsg)
    message(logmsg, appendLF=FALSE)
    logprint_file(logmsg)
  }
  
  invisible()
}



logfile_init <- function()
{
  if (file.exists(logfile))
    file.remove(logfile)
  
  logfile
}



logprint_file <- function(logmsg)
{
  cat(logmsg, file=logfile, append=TRUE)
  utils::flush.console()
  invisible()
}



#' showlog
#' 
#' Show the server log on the client.
#' 
#' @export
showlog <- function()
{
  if (file.exists(logfile))
    log = readLines(logfile)
  else
    stop("no log file found!")
  
  c(
    paste("### logfile:", logfile),
    log
  )
}
