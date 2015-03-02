library(rzmq)
source("../R/repl.r")

pbdenv$whoami <- "remote"
pbdenv$bcast_method <- "zmq"

#debug(pbd_eval)
pbd_repl()
