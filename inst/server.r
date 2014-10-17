library(pbdMPI)
init()


comm.print("Setting up servers...")

if (comm.rank() == 0) {
  library(shiny)
  library(shinyAce)
  library(pbdinline)
  runApp("~/dev/pbd/inline/inst/server/")
} else {
  while(TRUE){
    code <- NULL
    code <- bcast(code, rank.source=0L)
    
    ret <- eval(parse(text=code), envir=.GlobalEnv)
  }
}


finalize()
