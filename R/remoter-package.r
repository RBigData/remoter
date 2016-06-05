#' remoter
#'
#' A package for running R commands on a remote R session.  With 
#' remoter, you can use RStudio running on your laptop to execute
#' commands on an R session running on, for example, Amazon's EC2.
#' 
#' @references Project URL: \url{https://github.com/RBigData/remoter}
#' @author Drew Schmidt and Wei-Chen Chen
#' 
#' @name remoter-package
#' 
#' @importFrom pbdZMQ zmq
#' @import assertthat
#' 
#' @importFrom stats runif
#' @importFrom utils capture.output globalVariables packageVersion help
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
