.onLoad <- function(libname, pkgname)
{
  ### Preload to global environment.
  invisible(eval(parse(text = "remoter:::init_state()")))

  ### Load and set sodium then generate public/private keys.
  test <- requireNamespace("sodium", quietly=TRUE)
  set(withsodium, test)
  if (test)
    generate_keypair()
  
  invisible()
}

