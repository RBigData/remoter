killhung <- function(pid)
{
  if ("windows" == tolower(Sys.info()["sysname"]))
    stop("doesn't work :[")
  else
    system(paste("kill", pid))
  
  invisible()
}


