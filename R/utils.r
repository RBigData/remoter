validate_port <- function(port)
{
  assert_that(is.count(port))
  assert_that(port < 65536)
  
  if (port < 49152)
    warning("You are strongly encouraged to use port values between 49152 and 65536. See '?pbdZMQ::random_port' for details.")
  
  TRUE
}



get_versions <- function()
{
  pkgs <- c("pbdZMQ", "remoter")
  ret <- lapply(pkgs, packageVersion)
  names(ret) <- pkgs
  
  ret
}



compare_versions <- function(client, server)
{
  if (client$pbdZMQ < server$pbdZMQ)
    return(FALSE)
  if (client$remoter < server$remoter)
    return(FALSE)
  
  TRUE
}
