magicmsg_checkfor_pw <- "68638009903952479362"

#' State management for the pbdR Client/Server
#' 
#' @export
pbdenv <- new.env()

reset_state <- function()
{
  # options
  pbdenv$prompt <- "remoteR"
  pbdenv$port <- 55555
  pbdenv$remote_addr <- "localhost"
  pbdenv$password <- NULL
  pbdenv$maxattempts <- 5
  
  
  # internals
  pbdenv$context <- NULL
  pbdenv$socket <- NULL
  pbdenv$debug <- FALSE
  pbdenv$verbose <- TRUE
  pbdenv$client_lasterror <- ""
  
  pbdenv$remote_context <- NULL
  pbdenv$remote_socket <- NULL
  
  # C/S state
  pbdenv$status <- list(
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



### just a pinch of sugar
get.status <- function(var)
{
  name <- as.character(substitute(var))
  pbdenv$status[[name]]
}

set.status <- function(var, val)
{
  name <- as.character(substitute(var))
  pbdenv$status[[name]] <- val
  invisible()
}


