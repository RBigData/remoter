### Do not export this function to NAMESPACE, but use server() to assign this
### function to .GlobalEnv.
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...){
  check <- eval(parse(text = "suppressMessages(require(ggplot2))"))

  if (check)
  {
    org.print <- eval(parse(text = "ggplot2:::print.ggplot"))
    org.print(x, newpage, vp, ...)
  }
  else
    remoter_warning("ggplot2 is not installed.")

  invisible()
}
