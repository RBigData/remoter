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
#' ### Regular R plotting
#' remoter> plot(1:5)
#' remoter> rpng.off()
#'
#' ### Manually open a remoting plotting device
#' remoter> rpng()
#' remoter> plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rpng.off()
#'
#' ### Work with ggplot2-like plotting
#' remoter> library(ggplot2)
#' remoter> g1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,
#' remoter+              color = Species)) +
#' remoter+       geom_point(aes(shape = Species))
#'
#' ### Method 1a
#' remoter> rpng()
#' remoter> print(g1)
#' remoter> rpng.off()
#'
#' ### Method 1b
#' remoter> rpng()
#' remoter> print(g1 + geom_line())
#' remoter> rpng.off()
#'
#' ### Method 1c
#' remoter> g1 + geom_smooth(method = "lm")
#'
#' ### Testing aside
#' remoter> rpng.new(plot(1:5))
#'
#' ### Method 1d
#' remoter> rpng.new(g1)
#'
#' ### Testing again
#' remoter> b <- function() plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rpng.new(b)
#'
#' ### Work with other plotting device
#' remoter> da <- data.frame(x = rnorm(100), y = rnorm(100))
#' remoter> g2 <- ggplot(da, aes(x, y)) + geom_point()
#'
#' ### Save on the server
#' remoter> pdf()
#' remoter> g2
#' remoter> print(g2 + geom_line())
#' remoter> dev.off()
#'
#' ### Testing for errors with ggplot
#' remoter> g3 <- ggplot(da, aes(x, yy)) + geom_point()
#' remoter> g3
#'
#' remoter> rpng()
#' remoter> print(g3)
#' remoter> rpng.off()
#'
#' ### Disconnect the server and keep it alive
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
    stop("filename should be in character.")
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

