#' has.sodium
#' 
#' Report if the sodium package is availabe for use.
#' 
#' @details
#' TODO
#' 
#' @export
has.sodium <- function()
{
  .pbdenv$withsodium
}



#' is.secure
#' 
#' Report if communications with the connected server are
#' encrypted.
#' 
#' @details
#' TODO
#' 
#' @export
is.secure <- function()
{
  .pbdenv$secure
}



generate_keypair <- function()
{
  .pbdenv$keys$private <- sodium::keygen()
  .pbdenv$keys$public <- sodium::pubkey(.pbdenv$keys$private)
  
  invisible()
}



getkey <- function(type)
{
  name <- as.character(substitute(type))
  stopifnot(name == "private" || name == "public" || name == "theirs")
  .pbdenv$keys[[name]]
}
