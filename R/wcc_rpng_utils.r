#' @param expr
#' An expression or a function generating a plot. This checks in the
#' following orders: expression or ggplot. The ggplot
#' are eval'd within the \code{rpng.new()}, while the expression is
#' eval'd at \code{parent.frame()}.
#' @rdname rDevices_rpng
#' @export
rpng.new <- function(expr, filename = NULL,
                     width = 587, height = 586, units = "px", pointsize = 12,
                     bg = "white", res = 96, ...)
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
    eval(parse(text = "assign('.rDevices', list(''), envir = .GlobalEnv)"))

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
      else if (is.gg.ggplot(expr))
        print(expr)
      else
        eval(expr, envir = parent.frame())
    }
    else
      eval(expr, envir = parent.frame())

    ### Call rpng.off() when expr is an object or a function.
    return(invisible(rpng.off(which = dv)))
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
  set.status(need_auto_rpng_off, FALSE)

  if (which == 1)
  {
    ret <- "dev.off(): Can not shut down device 1 (the null device)."
    return(ret)
  }

  ### The ggplot below does NOT need auto_rpng_off but need a dev.off() later.
  ### remoter> g <- ggplot(da, aes(x, y)) + geom_point()
  ### remoter> pdf()
  ### remoter> g
  ### remoter>dev.off()
  if (iam("remote") && inwhileloop("server") && is.rpng.open())
  {
    ### Overwrite native R functions.
    set.status(need_auto_rpng_off, TRUE)
    grDevices::dev.off(which = which)
    filename <- .GlobalEnv$.rDevices[[which]]
    .GlobalEnv$.rDevices[[which]] <- ''
    ret <- png::readPNG(filename)
    return(invisible(ret))
  }

  ### For normal cases, such as interactive and file devices, one may call
  ### dev.off() to turn off the display, so to keep consistent I need to
  ### call/return the native R function because dev.off() is hijack.
  ret <- grDevices::dev.off(which = which)
  return(ret)
}



#' @rdname rDevices_rpng
#' @export
dev.off <- rpng.off



### This is called only when .pbdenv$status$need_auto_rpng_off is TRUE.
auto_rpng_off_local <- function(img)
{
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
    graphics::rasterImage(img.r, 0, 0, img.dim[1], img.dim[2])
  }
  else
  {
    eval(parse(text = "assign('.rpng.img', img, envir = .GlobalEnv)"))
    cat("Check 'local' .GlobalEnv$.rpng.img for the incorrect (raster) image.\n")
  }
}



is.gg.ggplot <- function(x)
{
  all(class(x) == c("gg", "ggplot"))
}

is.rpng.open <- function(which = grDevices::dev.cur())
{
   filename <- .GlobalEnv$.rDevices[[which]]
   return(!is.null(filename) && filename != '')
}

