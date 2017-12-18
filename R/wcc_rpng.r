#' rpng
#' 
#' Remote R PNG Device
#'
#' @description
#' Provide a graphic device locally for plots generated on server of Remote R
#' 
#' \code{rpng()} generates locally a device/window.
#'
#' \code{rpng.new()} generates locally a device/window.
#'
#' \code{rpng.off()} turns off locally a device/window.
#'
#' \code{dev.off()} is an alias of \code{rpng.off()} in order to consisten
#' with th original device function \code{grDevices::dev.off()}.
#'
#' @param filename
#' A temporary file to save the plot on server
#' @param width
#' width of the plot as in \code{grDevices::png()}
#' @param height
#' height of the plot as in \code{grDevices::png()}
#' @param units
#' units of the width and height as in \code{grDevices::png()}
#' @param pointsize
#' pointsze of the plotted text as in \code{grDevices::png()}
#' @param bg
#' background colour as in \code{grDevices::png()}
#' @param res
#' resolution as in \code{grDevices::png()}
#' @param ...
#' additional arguments as in \code{grDevices::png()}
#'  
#'
#' @seealso \code{\link{rDevices}}
#'
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs
#' ### remotely
#' > # suppressMessages(library(remoter, quietly = TRUE))
#' > # client()
#' > remoter::client("192.168.56.101")
#'
#' remoter> plot(1:5)
#' remoter> rpng.off()
#'
#' remoter> rpng()
#' remoter> plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rpng.off()
#'
#' remoter> library(ggplot2)
#' remoter> g1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,
#' remoter+              color = Species)) +
#' remoter+       geom_point(aes(shape = Species))
#' remoter> rpng()
#' remoter> print(g1)
#' remoter> rpng.off()
#'
#' remoter> g1 + geom_smooth(method = "lm")
#'
#' remoter> rpng.new(plot(1:5))
#'
#' remoter> rpng.new(g1)
#'
#' remoter> b <- function() plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rpng.new(b)
#'
#' remoter> da <- data.frame(x = rnorm(100), y = rnorm(100))
#' remoter> g2 <- ggplot(da, aes(x, y)) + geom_point()
#' remoter> g2
#'
#' remoter> pdf()
#' remoter> g2
#' remoter> print(g2 + geom_line())
#' remoter> dev.off()
#'
#' remoter> q()
#' >
#' }
#' 
#' @rdname rDevices_rpng
#' @name rpng
NULL

#' @export
rpng <- function(filename = tempfile(fileext = "_r.png"),
                 width = 587, height = 586, units = "px", pointsize = 12,
                 bg = "white", res = 96, ...)
{
  if (!is.character(filename))
    cat("filename should be in character.")
  else
  {
    ### Use NULL to delay opening a local device automatically
    rpng.new(NULL, filename = filename, width = width, height = height,
             units = units, pointsize = pointsize, bg = bg, res = res,
             ...)
    ### Use rpng.off() to close the remoter graphic device and
    ### open the local device manually.
  }

  invisible()
}

