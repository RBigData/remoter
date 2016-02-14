#' has.sodium
#' 
#' Report if the sodium package is availabe for use.
#' 
#' @return
#' Returns \code{TRUE} if the sodium package is available, and 
#' \code{FALSE} otherwise.
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
#' @return
#' Returns \code{TRUE} if messages between client and server are
#' currently encrypted, and \code{FALSE} if not.  If the client 
#' is not currently running (i.e., if executed from just a regular
#' R prompt), then \code{NA} is returned.
#' 
#' @export
is.secure <- function()
{
  if (iam("local"))
    NA
  else
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
