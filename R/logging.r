logprint <- function(msg, checkverbose=FALSE, preprint="")
{
  if (.pbdenv$serverlog)
  {
    if (!checkverbose || .pbdenv$verbose)
    {
      logmsg <- paste0(preprint, "[", Sys.time(), "]: ", msg, "\n")
      cat(logmsg)
      logfile(logmsg)
    }
  }
  
  invisible()
}



logfile <- function(logmsg)
{
  log <- paste0(tools::file_path_as_absolute("~"), "/.remoterserverlog")
  cat(logmsg, file=log, append=file.exists(log))
  invisible()
}
