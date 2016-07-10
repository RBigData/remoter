#' Local Graphic Devices
#' 
#' Local Graphic Device Controlling Functions
#' 
#' @description
#'
#' Functions for controlling graphic device locally when the client of
#' remote R is on. All these functions are evaluated in local R from within
#' the remote R prompt.
#'
#' \code{dev.curc()} locally evals \code{grDevices::dev.cur()}.
#'
#' \code{dev.listc()} locally evals \code{grDevices::dev.list()}.
#'
#' \code{dev.nextc()} locally evals \code{grDevices::dev.next()}.
#'
#' \code{dev.prevc()} locally evals \code{grDevices::dev.prev()}.
#'
#' \code{dev.offc()} locally evals \code{grDevices::dev.off()}.
#'
#' \code{dev.setc()} locally evals \code{grDevices::dev.set()}.
#'
#' \code{dev.newc()} locally eval \code{grDevices::dev.new()}.
#'
#' \code{dev.sizec()} locally evals \code{grDevices::dev.size()}.
#'
#'
#' @param which
#' An integer specifying a device number as in \code{grDevices::dev.off()}
#' @param ...
#' arguments to be passed to the device selected as in
#'  \code{grDevices::dev.new()}
#' @param noRstudioGD
#' as in \code{grDevices::dev.new()}
#' @param units
#' as in \code{grDevices::dev.size()}
#' 
#' @seealso \code{\link{rpng}()}
#'
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs
#' ### remotely
#' > library(remoter, quietly = TRUE)
#' > client()
#'
#' remoter> rpng.new(plot(1:5))
#' remoter> dev.newc(width = 6, height = 4)
#' remoter> a <- function() plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rpng.new(a, width = 6 * 72, height = 4 * 72)
#'
#' remoter> dev.curc()
#' remoter> dev.listc()
#' remoter> dev.offc()
#'
#' remoter> q()
#' >
#' }
#' 
#' @rdname rDevices
#' @name rDevices
NULL



#' @rdname rDevices
#' @export
dev.curc <- function()
{
  evalc(grDevices::dev.cur())
}



#' @rdname rDevices
#' @export
dev.listc <- function()
{
  evalc(grDevices::dev.list())
}



#' @rdname rDevices
#' @export
dev.nextc <- function(which = grDevices::dev.cur())
{
  evalc(grDevices::dev.next(which = which))
}



#' @rdname rDevices
#' @export
dev.prevc <- function(which = grDevices::dev.cur())
{
  evalc(grDevices::dev.prev(which = which))
}



#' @rdname rDevices
#' @export
dev.offc <- function(which = grDevices::dev.cur())
{
  if(iam("local"))
    tryCatch(grDevices::dev.off(which = which))
}



#' @rdname rDevices
#' @export
dev.setc <- function(which = grDevices::dev.cur())
{
  evalc(grDevices::dev.set(which = which))
}



#' @rdname rDevices
#' @export
dev.newc <- function(..., noRstudioGD = FALSE)
{
  if(iam("local"))
    tryCatch(grDevices::dev.new(..., noRstudioGD = noRstudioGD))
}



#' @rdname rDevices
#' @export
dev.sizec <- function(units = c("in", "cm", "px"))
{
  evalc(grDevices::dev.size(units = units))
}



### For windows only?
# bringToTopc <- function(which = grDevices::dev.cur(), stay = FALSE){
#   if(iam("local")){
#     tryCatch(grDevices::bringToTop(which = which, stay = stay))
#   }
# }

