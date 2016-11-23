### NOTE: all explicit references to .pbdenv should occure only in this file.
### To reference/modify state data, use the helper functions defined below.

magicmsg_first_connection <- ".__remoter_first_connection"


init_state <- function(envir = .GlobalEnv)
{
  if (!exists(".pbdenv", envir = envir))
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
  set(clientpw, NULL)
  set(maxattempts, 5)
  set(isbatch, FALSE)
  
  # logs
  set(serverlog, TRUE)
  set(verbose, FALSE)
  set(showmsg, FALSE)
  
  # internals
  set(debug, FALSE)
  set(context, NULL)
  set(socket, NULL)
  set(client_lasterror, "")
  
  # Exiting stuff
  set(client_called_exit, FALSE)
  set(client_called_shutdown, FALSE)
  set(kill_interactive_server, TRUE)
  
  
  # Crypto
  # .pbdenv$withsodium <- FALSE
  set(secure, FALSE)
  # .pbdenv$keys$private <- NULL
  # .pbdenv$keys$public <- NULL
  .pbdenv$keys$theirs <- NULL
  
  
  # Track assignments
  # set(objs, character(0))
  set(sync, FALSE)
  set(objs_nm, new.env())
  set(objs, new.env())
  
  
  # C/S state
  .pbdenv$status <- list(
    ret                = invisible(),
    ret_addition       = invisible(),
    visible            = FALSE,
    lasterror          = NULL,
    shouldwarn         = FALSE,
    num_warnings       = 0,
    warnings           = NULL,
    prompt_active      = FALSE,
    should_exit        = FALSE,
    continuation       = FALSE,
    need_auto_rpng_off = FALSE,
    need_auto_rhelp_on = FALSE,
    remote_objs        = NULL
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

inwhileloop <- function(name)
{
  ### Check if in the client/server while(TRUE) loops.
  all.calls <- base::sys.calls()
  match.call <- paste0("^(\\s+)?remoter_repl_", name, "\\(")
  check <- grepl(x=all.calls, pattern=match.call, perl=TRUE)
  any(check)
}
