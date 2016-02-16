readline_masked <- function(msg)
{
  ret <- .Call("readline_masked", msg)
  
  ret
}



remoter_read_password <- function()
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
