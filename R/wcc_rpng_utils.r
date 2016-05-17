
#' @param expr
#' An expression or a function generating a plot. This checks in the
#' following orders: expression or ggplot. The ggplot
#' are eval'd within the \code{rpng.new()}, while the expression is
#' eval'd at \code{parent.frame()}.
#' @rdname rDevices_rpng
#' @export
rpng.new <- function(expr, filename = NULL,
                     width = 587, height = 586, units = "px", pointsize = 12,
                     bg = "white", res = 96, interpolate = FALSE, ...)
{
  if (iam("remote"))
  {
    if (is.null(filename))
      filename <- tempfile(fileext = "_r.png")

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

    ### Plot and sent to remote if expr is not NULL.
    ### Do not check on "language", but "symbol" is ok.
    tmp <- substitute(expr)
    if (!is.null(tmp))
    {
      if (is.symbol(tmp))
      {
        if (is.function(expr))
          expr()
        else if (all(class(expr) == c("gg", "ggplot")))
          print(expr)
        else
          eval(expr, envir = parent.frame())
      }
      else
        eval(expr, envir = parent.frame())

      rpng.off.remote(which = dv)
    }
  }
  else if (iam("local"))
  {
    ### This will create a new device in local.
    # grDevices::dev.new(width = round(width / res),
    #                    height = round(height / res))

    ### Optain the opened device from remote if expr is not NULL
    tmp <- substitute(expr)
    if (!is.null(tmp))
      rpng.off.local(interpolate = interpolate)
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
    rpng.off.remote(which)
  else if (iam("local"))
    rpng.off.local()

  invisible()
}

rpng.off.remote <- function(which)
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
    ### close a regular plot
    grDevices::dev.off(which = which)

    ### sent remoter device to local
    if (exists(".rDevices", envir = .GlobalEnv) &&
        !is.null(.GlobalEnv$.rDevices[[which]]))
    {
        filename <- .GlobalEnv$.rDevices[[which]]
        img <- png::readPNG(filename)
        remoter_send(data = img, send.more = TRUE)
        .GlobalEnv$.rDevices[[which]] <- NULL
    }
  }
}

rpng.off.local <- function(interpolate = TRUE)
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
    cat("dev.off(): Can not shut down device 1 (the null device).\n")
  else
    cat("Check .GlobalEnv$.rpng.img for the incorrect (raster) image.\n")
}


#' @rdname rDevices_rpng
#' @export
dev.off <- rpng.off

