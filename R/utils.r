killhung <- function(pid)
{
  os <- get.os()
  
  if (same.str(os, "windows"))
    stop("doesn't work :[")
  else
    system(paste("kill", pid))
  
  invisible()
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
