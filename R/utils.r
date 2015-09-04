kill <- function(pid)
{
  os <- get.os()
  
  if (same.str(os, "windows"))
    stop("doesn't work :[")
  else
  {
    if (checkpid(pid))
      system(paste("kill", pid))
    else
      warning(paste("pid=", pid, " does not exist", sep=""))
  }
  
  invisible()
}


checkpid <- function(pid)
{
  os <- get.os()
  
  if (same.str(os, "windows"))
    stop("doesn't work :[")
  else
  {
    x <- suppressWarnings(system(paste("kill -0", pid, "2>&1"), intern=TRUE))
    
    match <- grep(x=x, pattern="No such process")
    if (length(match) == 0)
      match <- 0L
    
    return( !as.logical(match) )
  }
}



is.int <- function(x)
{
  if (is.numeric(x))
  {
    if (x-as.integer(x) == 0)
      return( TRUE )
    else
      return( FALSE )
  }
  else
    return( FALSE )
}



same.str <- function(str1, str2)
{
  return( tolower(str1) == tolower(str2) )
}



get.os <- function()
{
  ret <- Sys.info()["sysname"]
  return( ret )
}



dirsep <- function()
{
  if (same.str(get.os(), "windows"))
    "\\"
  else
    "/"
}


