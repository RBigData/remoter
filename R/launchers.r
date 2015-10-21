#' @export
server <- function(port=55555, showmsg=FALSE)
{
  pbdenv$whoami <- 'remote'
  pbdenv$port <- port
  pbdenv$debug <- showmsg
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

