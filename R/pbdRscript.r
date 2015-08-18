#' pbdRscript
#' 
#' Simple tool for executing pbdR batch jobs from inside R.
#' 
#' @description
#' This function is a simple wrapper around the system() command.  As such,
#' data is not shared between the calling R process and the batch processes
#' which execute the 'body' source.
#' 
#' @param body 
#' character; the pbdR script to be evaluated.
#' @param nranks 
#' The number of MPI ranks to launch.
#' @param auto
#' logical; determines if the script should automatically load the
#' pbdMPI package, and call init() (at the beginning) and finalize() (at the
#' end).
#' @param auto.dmat 
#' logical; determines if the script should automatically load
#' the pbdDMAT package and call init.grid(). Automatically sets \code{auto=TRUE}.
#' @param pid
#' Logical; determines if the process id (pid) should be printed to the
#' terminal or not. This is useful if you need to kill a hung job.
#' @param wait
#' Logical values passed to R's \code{system()}.
#'
#' @details
#' This is a simple wrapper around a system call to mpirun on the
#' input script.
#' 
#' @export
pbdRscript <- function(body, nranks=1, auto=TRUE, auto.dmat=FALSE,
    pid=TRUE, wait=TRUE)
{
  ### Input checks
  # if (same.str(get.os(), "windows"))
  #   stop("You can't use this on Windows")
  
  if (!is.character(body))
    stop("argument 'body' must be a character string")
  else if (length(body) == 0)
    stop("argument 'body' must be a non-empty character string")
  else if (length(body) > 1)
  {
    warning("function body has length > 1; only the first element will be used")
    body <- body[1L]
  }
  
  if (!is.int(nranks))
    stop("argument 'nranks' must be an integer")
  if (!is.logical(auto))
    stop("argument 'auto' must be logical")
  if (!is.logical(auto.dmat))
    stop("argument 'auto.dmat' must be logical")
    
  
  ### Dump body to temp file, execute
  if (auto.dmat)
    auto <- TRUE
  
  if (auto)
  {
    auto.header <- "suppressPackageStartupMessages(library(pbdMPI, quietly=TRUE))\ninit()\n\n"
    
    if (auto.dmat)
      auto.header <- paste(auto.header, "\n", "suppressPackageStartupMessages(library(pbdDMAT, quietly=TRUE))\ninit.grid()\n\n")
    
    auto.footer <- "\n\nfinalize()"
    body <- paste(auto.header, body, auto.footer, collapse="\n")
  }
  
  ### Create a temp file for pbdR servers.
  script <- tempfile()
  if (same.str(get.os(), "windows"))
  {
    script <- gsub("\\\\", "/", script)
    script.bat <- paste0(script, ".bat") 
  }

  ### Dump demon script to temp file for pbdR servers.
  conn <- file(script, open="wt")
  writeLines(paste0(".__the_pbd_script <- \"", script, "\""), conn)
  writeLines(body, conn)
  writeLines("unlink(.__the_pbd_script)", conn)
  close(conn)
  
  if (pbdenv$debug)
    cat("server tmpfile:  ", script, "\n")
  
  ### Launch mpi commands.
  if (!same.str(get.os(), "windows"))
  {
    cmd <- paste("mpirun -np", nranks, "Rscript", script)
    if (pid)
      cmd <- paste(cmd, "&\necho \"PID=$!\n")

    ### Run system shell command.
    ret <- system(cmd, intern=FALSE, wait=wait)
  }
  else
  {
    ### Dump command to a windows batch file.
    cmd <- paste0("mpiexec -np ", nranks, " Rscript ", script, "\n")
    conn.bat <- file(script.bat, open="wt")
    writeLines(cmd, conn.bat)
    close(conn.bat)
    script.bat <- sub("^\\./", "", script.bat)

    ### Run system batch command via shell.exec.
    if (!is.loaded("shellexec_wcc", PACKAGE = "pbdZMQ", type = "Call"))
    {
      ret <- shell.exec(script.bat)
    } else{
      ret <- shellexec.wcc(script.bat)
    }
  }
  
  invisible()
}

