# remoter> rpng.new(plot(1:5))
# remoter> b <- function() plot(iris$Sepal.Length, iris$Petal.Length)
# remoter> rpng.new(b, bg = "transparent")

#' @param expr
#' An expression or a function generating a plot. This checks in the
#' following orders: function, ggplot, expression. The function and ggplot
#' are eval'd within the \code{rpng.new()}, while the expression is
#' eval'd at \code{parent.frame()}.
#' @rdname rDevices_rpng
#' @export
rpng.new <- function(expr, filename = NULL,
                     width = 590, height = 590, units = "px", pointsize = 12,
                     bg = "white", res = 120, interpolate = FALSE, ...)
{
  if (iam("remote"))
  {
    if (is.null(filename))
    {
      filename <- tempfile(fileext = "_r.png")
    }

    ### Open a device
    grDevices::png(filename = filename, width = width, height = height,
                   units = units, pointsize = pointsize, bg = bg, res = res,
                   ...)
    graphics::plot.new()
    dv <- grDevices::dev.cur()

    ### Assign the opened device file name
    if (!exists(".rDevices", envir = .GlobalEnv))
      eval(parse(text = "assign('.rDevices', list(), envir = .GlobalEnv)"))
    .GlobalEnv$.rDevices[[dv]] <- filename

    ### Check function and ggplot first. Otherwise eval it in parent.frame().
    if (is.function(expr))
      expr()
    else if (all(class(expr) == c("gg", "ggplot")))
      print(expr)
    else if (!is.null(expr))
      eval(expr, envir = parent.frame())

    ### Close the opened device and sent to local if expr is not NULL
    if (!is.null(expr))
    {
      grDevices::dev.off(which = dv)
      img <- png::readPNG(filename)
      remoter_send(data = img, send.more = TRUE)
    }

    invisible(filename)
  }
  else if (iam("local"))
  {
    ### Optain the opened device from remote if expr is not NULL
    if (!is.null(expr))
    {
      img <- remoter_receive()
      eval(parse(text = "assign('.rpng.img', img, envir = .GlobalEnv)"))

      if (is.array(img))
      {
        ### Semi-transparency may not work in windows device
        # if (.Platform$OS.type == "windows")
        #   img.r <- grDevices::as.raster(img[,, 1:3])
        # else
        img.r <- grDevices::as.raster(img)
    
        ### This will create a new device in local.
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
        graphics::rasterImage(img.r, 0, 0, img.dim[1], img.dim[2],
                              interpolate = interpolate)
      }
      else if (is.na(img))
        warning("dev.off(): Can not shut down device 1 (the null device).")
      else
        warning("Check .GlobalEnv$.rpng.img for the incorrect (raster) image.")
    }
  }

  invisible()
}

#' @param which
#' An integer specifying a device number as in \code{grDevices::dev.off()}
#'
#' @rdname rDevices_rpng
#' @export
rpng.off <- function(which = grDevices::dev.cur())
{
  if (iam("remote"))
  {
    ### Avoid null device
    if (which == 1)
    {
      cat("dev.off(): Can not shut down device 1 (the null device).\n")
      ### local need to know the device 1 is being closed in remote because
      ### which may not be 1 in local. Send a NA to maintain the communication
      ### stage.
      remoter_send(data = NA, send.more = TRUE)
    }
    else
    {
      if (exists(".rDevices", envir = .GlobalEnv))
      {
        if(!is.null(.GlobalEnv$.rDevices[[which]]))
        {
          ### sent remoter device to local
          filename <- .GlobalEnv$.rDevices[[which]]
          .GlobalEnv$.rDevices[[which]] <- NULL

          grDevices::dev.off(which = which)
          img <- png::readPNG(filename)
          remoter_send(data = img, send.more = TRUE)
        }
        else
          grDevices::dev.off(which = which)   ### close a regular plot
      }
      else
        grDevices::dev.off(which = which)   ### close a regular plot
    }
  }
  else if (iam("local"))
    rpng.new(NA, filename = NULL)    ### receive/display a remote plot

  invisible()
}

#' @rdname rDevices_rpng
#' @export
dev.off <- rpng.off

