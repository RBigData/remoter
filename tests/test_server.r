source("../R/repl.r")

pbdenv$whoami <- "remote"

#debug(pbd_eval)
pbd_repl()
