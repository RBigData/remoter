readline_masked <- function(msg)
{
  ret <- .Call("readline_masked", msg)
  
  ret
}



#' Password Input
#' 
#' Masked user input (where supported; see details section) for
#' entering passwords.
#' 
#' @details
#' Masking (i.e., not displaying the password or "*" characters
#' as input is provided) is supported on sevreal, but not all
#' platforms.  It is supported in RStudio, provided you
#' have a suitable version of the GUI and of the package 
#' 'rstudioapi'.  It should also work in the terminal on any
#' OS.  It is *NOT* supported in the Windows GUI RGui or on
#' the Mac GUI R.app.
#' 
#' @export
password <- function()
{
  query <- "PASSWORD:  "
  
  if (getval(withrstudioapi) && rstudioapi::hasFun("askForPassword"))
    pw <- rstudioapi::askForPassword(query)
  else if (.Platform$GUI == "X11" || .Platform$GUI == "RTerm")
    pw <- readline_masked(query)
  else
  {
    cat("WARNING: input is not masked!\n")
    pw <- readline(query) 
  }
  
  pw
}
