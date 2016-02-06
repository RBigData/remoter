.onLoad <- function(libname, pkgname)
{
  test <- requireNamespace("sodium", quietly=TRUE)
  if (test)
  {
    generate_keypair()
    set(withsodium, TRUE)
  }
  else
    set(withsodium, FALSE)
  
  invisible()
}



### doesn't resolve in the right order...
# .onAttach <- function(libname, pkgname)
# {
#   if (!has.sodium())
#     packageStartupMessage("\nremoter NOTE: Unable to load 'sodium'. You will not be able to launch or connect to secure servers.")
# }
