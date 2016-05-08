#' rrpng
#' 
#' Remote R PNG Device
#' 
#' @description
#' Provide a graphic device locally for plots generated on server of Remote R
#'
#' @param expr
#' An expression or a function generating a plot. This checks in the
#' following orders: function, ggplot, expression. The function and ggplot
#' are eval'd within the \code{rrpng()}, while the expression is
#' eval'd at \code{parent.frame()}.
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
#' @return
#' A local plot
#'
#' @seealso \code{\link{rrDevices}}
#'
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs
#' ### remotely
#' > library(remoter, quietly = TRUE)
#' > client()
#'
#' remoter> rrpng(plot(1:5))
#' remoter> b <- function() plot(iris$Sepal.Length, iris$Petal.Length)
#' remoter> rrpng(b, bg = "transparent")
#'
#' remoter> library(ggplot2)
#' remoter> g <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
#' remoter+      geom_point(aes(colour = Species, shape = Species))
#' remoter> rrpng(g)
#' remoter> dev.newc(width = 6, height = 4)
#' remoter> rrpng(g, width = 6 * 72, height = 4 * 72, bg = "transparent")
#'
#' remoter> q()
#' >
#' }
#' 
#' @rdname rrDevices_png

#' @export
rrpng <- function(expr, filename = tempfile(fileext = "_rr.png"),
                  width = 480, height = 480, units = "px", pointsize = 12,
                  bg = "white", res = NA, ...){
  if (iam("remote"))
  {
    grDevices::png(filename = filename, width = width, height = height,
                   units = units, pointsize = pointsize, bg = bg, res = res,
                   ...)
    graphics::plot.new()
    dv <- grDevices::dev.cur()

    ### Check function and ggplot first. Otherwise eval it in parent.frame().
    if (is.function(expr))
      expr()
    else if (all(class(expr) == c("gg", "ggplot")))
      print(expr)
    else
      eval(expr, envir = parent.frame())

    grDevices::dev.off(which = dv)
    img <- png::readPNG(filename)
    remoter_send(data = img, send.more = TRUE)
    invisible(filename)
  }
  else if (iam("local"))
  {
    img <- remoter_receive()
    eval(parse(text = "assign('.rrpng.img', img, envir = .GlobalEnv)"))

    if (is.array(img))
    {
      ### Semi-transparency may not work in windows device
      # if (.Platform$OS.type == "windows")
      #   img.r <- grDevices::as.raster(img[,, 1:3])
      # else
      img.r <- grDevices::as.raster(img)
    
      ### This will create a new device.
      # if (is.na(res)) res <- 72
      # grDevices::dev.new(width = round(width / res),
      #                    height = round(height / res))
      ### This will overwrite the device if one is there, otherwise a new
      ### device will be created.
      graphics::plot.new()

      ### Set a empty plot.
      graphics::par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
      img.dim <- dim(img.r)
      graphics::plot(NULL, NULL, type = "n", axes = FALSE,
                     main = "", xlab = "", ylab = "",
                     xlim = c(0, img.dim[1]), ylim = c(0, img.dim[2]))
      graphics::rasterImage(img.r, 0, 0, img.dim[1], img.dim[2])
    }
    else
    {
      warning("Check .GlobalEnv$.rrpng.img for the incorrect image.")
    }
  }
}

