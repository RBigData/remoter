logprint <- function(msg, checkverbose=FALSE, checkshowmsg=FALSE, preprint="", level="")
{
  if (identical(msg, magicmsg_first_connection))
    return(invisible())
  
  if (.pbdenv$serverlog || (.pbdenv$showmsg && checkshowmsg))
  {
    if (!checkverbose || !checkshowmsg || .pbdenv$verbose)
    {
      logmsg <- paste0(preprint, "[", Sys.time(), "]: ", level, ifelse(level=="", "", ": "), msg, "\n")
      cat(logmsg)
      # logprint_file(logmsg)
    }
  }
  
  invisible()
}



logfile <- function()
{
  if (is.null(.pbdenv$logfile))
  {
    log <- paste0(tools::file_path_as_absolute("~"), "/.remoterserverlog")
    .pbdenv$logfile <- log
  }
  else
    log <- .pbdenv$logfile
    
  log
}



logprint_file <- function(logmsg)
{
  cat(logmsg, file=log, append=file.exists(log))
  invisible()
}
