logprint <- function(msg, checkverbose=FALSE, checkshowmsg=FALSE, preprint="", level="")
{
  if (identical(msg, magicmsg_first_connection))
    return(invisible())
  
  if ((.pbdenv$serverlog && !checkverbose && !checkshowmsg) || (.pbdenv$verbose && checkverbose) || (.pbdenv$showmsg && checkshowmsg))
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
  cat(logmsg, file=.pbdenv$logfile, append=TRUE)
  invisible()
}



#' showlog
#' 
#' Show the server log on the client.
#' 
#' @export
showlog <- function()
{
  file <- .pbdenv$logfile
  if (file.exists(file))
    readLines(.pbdenv$logfile)
  else
    stop("no log file found!")
}

