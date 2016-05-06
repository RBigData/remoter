#' rrpng
#' 
#' Remote R PNG Device
#' 
#' @description
#' Provide a graphic device locally for plots generated on server of Remote R
#'
#' @param func
#' A function generates a plot
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
#' 
#' @return
#' A local plot
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
#' remoter> dev.offc(2)
#' remoter> dev.offc(3)
#' remoter> q()
#' }
#' 
#' @rdname rrDevices_png

#' @export
rrpng <- function(func = NULL, filename = tempfile(fileext = "_rr.png"),
                  width = 480, height = 480, units = "px", pointsize = 12,
                  bg = "white", res = NA, ...){
  if(iam("remote")){
    grDevices::png(filename = filename, width = width, height = height,
                   units = units, pointsize = pointsize, bg = bg, res = res,
                   ...)
    graphics::plot.new()
    dv <- dev.cur()
    func()
    dev.off(which = dv)
    img <- png::readPNG(filename)
    remoter_send(data = img, send.more = TRUE)
    invisible(filename)
  } else if(iam("local")){
    img <- remoter_receive()
    img.r <- as.raster(img[,, 1:3])
    img.dim <- dim(img.r)
    graphics::plot.new()
    graphics::par(mar = rep(0, 4))
    graphics::plot(NULL, NULL, type = "n", axes = FALSE,
                   main = "", xlab = "", ylab = "",
                   xlim = c(0, img.dim[1]), ylim = c(0, img.dim[2]))
    graphics::rasterImage(img.r, 0, 0, img.dim[1], img.dim[2])
  }
}

