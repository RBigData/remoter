validate_port <- function(port)
{
  assert_that(is.count(port))
  assert_that(port < 65536)
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
