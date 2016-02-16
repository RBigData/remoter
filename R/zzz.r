.onLoad <- function(libname, pkgname)
{
  ### Preload to global environment.
  invisible(eval(parse(text = "remoter:::init_state()")))

  ### Load and set sodium then generate public/private keys.
  test <- requireNamespace("sodium", quietly=TRUE)
  if (test)
  {
    generate_keypair()
    set(withsodium, TRUE)
  }
  else
    set(withsodium, FALSE)
  
  test <- requireNamespace("rstudioapi", quietly=TRUE)
  if (test)
    set(withrstudioapi, TRUE)
  else
    set(withrstudioapi, FALSE)
  
  invisible()
}



### doesn't resolve in the right order...
# .onAttach <- function(libname, pkgname)
# {
#   if (!has.sodium())
#     packageStartupMessage("\nremoter NOTE: Unable to load 'sodium'. You will not be able to launch or connect to secure servers.")
# }
