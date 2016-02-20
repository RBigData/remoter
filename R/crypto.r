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
  getval(withsodium)
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
    getval(secure)
}



generate_keypair <- function()
{
  setkey(private, sodium::keygen())
  setkey(public, sodium::pubkey(getkey(private)))
  
  invisible()
}
