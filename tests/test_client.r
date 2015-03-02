library(rzmq)
source("../R/repl.r")

pbdenv$whoami <- "local"

#debug(pbd_eval)
pbd_repl()
