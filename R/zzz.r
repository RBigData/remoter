.onLoad <- function(libname, pkgname)
{
  test <- tryCatch(require("sodium", quietly=TRUE), warning=identity)
  if (inherits(test, c("simpleWarning", "warning")))
  {
    packageStartupMessage("\nremoter NOTE: Unable to load 'sodium'. You will not be able to launch or connect to secure servers.")
    set(withsodium, FALSE)
  }
  else
  {
    generate_keypair()
    set(withsodium, TRUE)
  }
  
  invisible()
}
