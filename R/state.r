magicmsg_first_connection <- ".__remoter_first_connection"

.pbdenv <- new.env()

reset_state <- function()
{
  # options
  .pbdenv$prompt <- "remoteR"
  .pbdenv$port <- 55555
  .pbdenv$remote_addr <- "localhost"
  .pbdenv$password <- NULL
  .pbdenv$maxattempts <- 5
  .pbdenv$checkversion <- TRUE
  
  # internals
  .pbdenv$serverlog <- TRUE
  .pbdenv$context <- NULL
  .pbdenv$socket <- NULL
  .pbdenv$debug <- FALSE
  .pbdenv$verbose <- TRUE
  .pbdenv$client_lasterror <- ""
  
  .pbdenv$remote_context <- NULL
  .pbdenv$remote_socket <- NULL
  
  # Crypto
  .pbdenv$keys$private <- NULL
  .pbdenv$keys$public <- NULL
  .pbdenv$keys$theirs <- NULL
  
  # C/S state
  .pbdenv$status <- list(
    ret               = invisible(),
    visible           = FALSE,
    lasterror         = NULL,
    shouldwarn        = FALSE,
    num_warnings      = 0,
    warnings          = NULL,
    remoter_prompt_active = FALSE,
    should_exit       = FALSE,
    continuation      = FALSE
  )
  
  invisible()
}



### Crypto
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



### just a pinch of sugar
set <- function(var, val)
{
  name <- as.character(substitute(var))
  .pbdenv[[name]] <- val
  invisible()
}

get.status <- function(var)
{
  name <- as.character(substitute(var))
  .pbdenv$status[[name]]
}

set.status <- function(var, val)
{
  name <- as.character(substitute(var))
  .pbdenv$status[[name]] <- val
  invisible()
}

iam <- function(name)
{
  .pbdenv$whoami == name
}

logprint <- function(msg)
{
  if (.pbdenv$serverlog)
    cat(paste0("[", Sys.time(), "]: ", msg, "\n"))
  
  invisible()
}
