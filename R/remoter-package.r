#' remoter
#'
#' A set of utilities for client/server computing with R, controlling
#' a remote R session (the server) from a local one (the client).  Simply set
#' up a server (see package vignette for more details) and connect to it from
#' your local R session ('RStudio', terminal, etc).  The client/server
#' framework is a custom 'REPL' and runs entirely in your R session without the
#' need for installing a custom environment on your system.  Network
#' communication is handled by the 'ZeroMQ' library by way of the 'pbdZMQ'
#' package.
#' 
#' @references Project URL: \url{https://github.com/RBigData/remoter}
#' @author Drew Schmidt and Wei-Chen Chen
#' 
#' @name remoter-package
#' 
#' @importFrom argon2 pw_hash pw_check
#' @importFrom stats runif
#' @importFrom utils capture.output globalVariables packageVersion help str
#' @importFrom tools file_path_as_absolute Rd2txt
#' @importFrom getPass getPass
#' @importFrom grDevices dev.cur dev.list dev.next dev.prev dev.off dev.set
#'             dev.new dev.size png as.raster
#' @importFrom graphics plot.new par plot rasterImage
#' @importFrom png readPNG
#' 
#' @docType package
#' @keywords package
NULL
