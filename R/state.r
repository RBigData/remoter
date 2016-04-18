magicmsg_first_connection <- ".__remoter_first_connection"



init_state <- function(envir = .GlobalEnv)
{
  if(!exists(".pbdenv", envir = envir))
    envir$.pbdenv <- new.env()
  
  reset_state()
  
  invisible()
}



reset_state <- function()
{
  # options
  set(prompt, "remoter")
  set(timer, FALSE)
  set(port, 55555)
  set(remote_addr, "localhost")
  set(password, NULL)
  set(maxattempts, 5)
  
  # logs
  set(serverlog, TRUE)
  set(verbose, FALSE)
  set(showmsg, FALSE)
  set(logfile, logfile_init())
  
  # internals
  set(debug, FALSE)
  set(context, NULL)
  set(socket, NULL)
  set(client_lasterror, "")
  set(kill_interactive_server, TRUE)
  
  
  # Crypto
  # .pbdenv$withsodium <- FALSE
  set(secure, FALSE)
  # .pbdenv$keys$private <- NULL
  # .pbdenv$keys$public <- NULL
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



### just a pinch of sugar
set <- function(var, val)
{
  name <- as.character(substitute(var))
  .pbdenv[[name]] <- val
  invisible()
}

getval <- function(var)
{
  name <- as.character(substitute(var))
  .pbdenv[[name]]
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

getkey <- function(type)
{
  name <- as.character(substitute(type))
  stopifnot(name == "private" || name == "public" || name == "theirs")
  .pbdenv$keys[[name]]
}

setkey <- function(var, val)
{
  name <- as.character(substitute(var))
  .pbdenv$keys[[name]] <- val
  invisible()
}

iam <- function(name)
{
  .pbdenv$whoami == name
}
