### client printing tools
remoter_client_stop <- function(msg)
{
  set(client_lasterror, msg)
  cat("Error: ", msg, "\n")
  
  invisible()
}



remoter_repl_printer <- function()
{
  ### cast addition first.
  addition <- get.status(ret_addition)
  if (!is.null(addition))
    cat(paste(addition, collapse = "\n"), "\n")

  ### cast return second.
  if (get.status(visible))
    cat(paste(get.status(ret), collapse="\n"), "\n")
  
  remoter_show_errors()
  remoter_show_warnings()
  
  return(invisible())
}



remoter_show_errors <- function()
{
  if (!is.null(get.status(lasterror)))
    cat(get.status(lasterror))
  
  invisible()
}



remoter_show_warnings <- function(force=FALSE)
{
  warnings <- get.status(warnings)
  nwarnings <- length(warnings)
  
  if (!is.null(warnings) && get.status(shouldwarn))
  {
    if (nwarnings == 1)
    {
      cat("Warning message:\n")
      cat(warnings)
    }
    else if (nwarnings < 11 || force)
    {
      cat("Warning messages:\n")
      for (i in 1:nwarnings)
      {
        w <- warnings[i]
        cat(paste0(i, ": ", w, "\n"))
      }
    }
    else
    {
      cat(paste("There were", nwarnings, "warnings (use warnings() to see them)"))
    }
    cat("\n")
  }
  
  set.status(visible, FALSE)
  set.status(shouldwarn, FALSE)
  
  invisible()
}
