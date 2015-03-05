library(rzmq)
source("../R/repl.r")

pbdenv$whoami <- "local"
pbdenv$debug <- TRUE

#debug(pbd_eval)
pbd_repl()
