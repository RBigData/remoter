#' rrDevices
#' 
#' Remote R Graphic Device Controling Functions
#' 
#' @description
#' Functions for controlling graphic device locally
#'
#' \code{dev.newc()} locally eval \code{grDevices::dev.new()}.
#'
#' \code{dev.offc()} locally evals \code{grDevices::dev.off()}.
#'
#' \code{dev.curc()} locally evals \code{grDevices::dev.cur()}.
#'
#' @param which
#' An integer specifying a device number as in \code{grDevices::dev.off()}
#' @param ...
#' arguments to be passed to the device selected as in
#'  \code{grDevices::dev.new()}
#' 
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs remotely
#' > library(remoter, quietly = TRUE)
#' > client()
#' remoter> a <- function() plot(1:5)
#' remoter> rrpng(a)
#' remoter> dev.newc()
#' remoter> b <- function() plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rrpng(b)
#' remoter> dev.curc()
#' remoter> dev.offc(2)
#' remoter> dev.offc(3)
#' remoter> q()
#' }
#' 
#' @rdname rrDevices

#' @export
dev.offc <- function(which = dev.cur()){
  if(iam("local")){
    tryCatch(grDevices::dev.off(which = which))
  }
}

#' @export
dev.newc <- function(..., noRstudioGD = FALSE){
  if(iam("local")){
    tryCatch(grDevices::dev.new(..., noRstudioGD = noRstudioGD))
  }
}

#' @export
dev.curc <- function(){
  evalc(grDevices::dev.cur())
}
