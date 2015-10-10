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
#' @param mpicmd
#' The command to launch mpi as a string (e.g., "mpirun", "mpiexec", 
#' "aprun", ...).
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
#' @param temp
#' A temporary file path that is accessible to all nodes, including
#' the client.  See details for a full explanation.
#'
#' @details
#' This is a simple wrapper around a system call to mpirun on the
#' input script.
#' 
#' Temp needs to be a file that the client and all servers can
#' read from.
#' 
#' @name pbdRscript
#' @rdname pbdRscript
NULL



#' @rdname pbdRscript
#' @export
pbdRscript_cmd <- function(body, mpicmd="mpirun", nranks=1, auto=TRUE, auto.dmat=FALSE,
    pid=TRUE, wait=TRUE, temp=tempfile())
{
  ### Input checks
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
    auto.header <- "suppressPackageStartupMessages(library(pbdMPI, quietly=TRUE))\n\n"
    
    if (auto.dmat)
      auto.header <- paste(auto.header, "\n", "suppressPackageStartupMessages(library(pbdDMAT, quietly=TRUE))\ninit.grid()\n\n")
    
    auto.footer <- "\n\nfinalize()"
    body <- paste(auto.header, body, auto.footer, collapse="\n")
  }

  ### Create a temp file for pbdR servers.
  script <- temp
  if (same.str(get.os(), "windows"))
  {
    script <- gsub("\\\\", "/", script)
    script.bat <- paste0(script, ".bat") 
  }
  
  ### Dump daemon script to temp file for pbdR servers.
  conn <- file(script, open="wt")
  writeLines(paste0(".__the_pbd_script <- \"", script, "\""), conn)
  writeLines(body, conn)
  writeLines("unlink(.__the_pbd_script)", conn)
  close(conn)
  
  if (pbdenv$debug)
    cat("server tmpfile:  ", script, "\n")
  
  
  cmd <- paste(mpicmd, "-np", nranks, "Rscript", script)
  cmd
}



#' @rdname pbdRscript
#' @export
pbdRscript <- function(body, mpicmd="mpirun", nranks=1, auto=TRUE, auto.dmat=FALSE,
    pid=TRUE, wait=TRUE, temp=tempfile())
{
  cmd <- pbdRscript_cmd(body, mpicmd, nranks, auto, auto.dmat, pid, wait, temp)
  
  ### Launch mpi commands.
  if (!same.str(get.os(), "windows"))
  {
    if (pid)
      cmd <- paste(cmd, "& echo \"PID=$!\n")
    
    ### Run system shell command.
    ret <- system(cmd, intern=FALSE, wait=wait)
  }
  else
  {
    ### Dump command to a windows batch file.
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

