.cwd <- getwd()

suppressMessages(library(pbdMPI, quietly = TRUE))
init()
### This does not solve the problem, but useful in some cases.
# .SPMD.CT$mpi.finalize <- FALSE


comm.print("Setting up servers...")

### Server and all clients need this.
suppressMessages(library(pbdinline, quietly = TRUE))
ret.check <- TRUE
.pbdCSEnv$log <- vector(mode = "list", .CS.CT$log.size)
.pbdCSEnv$log.counter <- 1
### Overwrite pbdMPI::finalize() and maybe q().
finalize <- function(){
  pbdMPI::finalize()
  base::q()
} # End of finalize().
quit <- function(){
  pbdMPI::finalize()
  base::quit()
} # End of quit().
q <- function(){
  pbdMPI::finalize()
  base::q()
} # End of q().


if (comm.rank() == 0) {
  ### Server
  library(shiny)
  library(shinyAce)
  # runApp("~/dev/pbd/inline/inst/server/")
  dir.server <- tools::file_path_as_absolute(
                  system.file("server", package = "pbdinline"))
  runApp(dir.server)
}

### All SPMD start here.
setwd(.cwd)
while(ret.check){
  code <- NULL
  code <- bcast(code, rank.source=0L)

  ### This is no need any more in shiny. Let shiny take care of try-catch.
  ret <- eval(parse(text = code), envir = .GlobalEnv)
  # .pbdCSEnv$log[[log.counter]] <-
  #   try(eval(parse(text = code), envir = .GlobalEnv), silent = TRUE)
  .pbdCSEnv$log[[.pbdCSEnv$log.counter]] <- code
  .pbdCSEnv$log.counter <- .pbdCSEnv$log.counter %% .CS.CT$log.size + 1

  ret.check <- !is.finalized()
}


finalize()
### This method is no help since the error is from one more bcast.
# finalize(mpi.finalize = TRUE)
