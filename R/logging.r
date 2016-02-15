logprint <- function(msg, checkverbose=FALSE, checkshowmsg=FALSE, preprint="", level="")
{
  if (identical(msg, magicmsg_first_connection))
    return(invisible())
  
  if ((getval(serverlog) && !checkverbose && !checkshowmsg) || (getval(verbose) && checkverbose) || (getval(showmsg) && checkshowmsg))
  {
    logmsg <- paste0(preprint, "[", Sys.time(), "]: ", level, ifelse(level=="", "", ": "), msg, "\n")
    cat(logmsg)
    logprint_file(logmsg)
  }
  
  invisible()
}



logfile_init <- function()
{
  logfile <- paste0(tools::file_path_as_absolute("~"), "/.remoterserverlog")
  
  if (file.exists(logfile))
    file.remove(logfile)
  
  logfile
}



logprint_file <- function(logmsg)
{
  cat(logmsg, file=getval(logfile), append=TRUE)
  invisible()
}



#' showlog
#' 
#' Show the server log on the client.
#' 
#' @export
showlog <- function()
{
  file <- getval(logfile)
  if (file.exists(file))
    readLines(getval(logfile))
  else
    stop("no log file found!")
}
