#' @export
server <- function(port=55555)
{
  pbdenv$whoami <- 'remote'
  pbdenv$port <- port
  remoter_repl()
  
  invisible(TRUE)
}

#' @export
client <- function(remote_addr, port=55555)
{
  pbdenv$whoami <- "local"
  pbdenv$port <- port
  pbdenv$remote_addr <- remote_addr
  remoter_repl()
  
  invisible(TRUE)
}

