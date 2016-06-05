#' rhelp
#' 
#' Remote R Help System
#'
#' @description
#' Provide the primary interface to the help systems as \code{utils::help()}
#' 
#' @param topic
#' A topic as in \code{utils::help()}
#' @param package
#' A package as in \code{utils::help()}
#' @param lib.loc
#' A lib location as in \code{utils::help()}
#' @param verbose
#' if verbose on/off as in \code{utils::help()}
#' @param try.all.packages
#' if try all packages as in \code{utils::help()}
#' @param help_type
#' only text is supported in \pkg{remoter}
#'
#' @examples
#' \dontrun{
#' ### Prompts are listed to clarify when something is eval'd locally vs
#' ### remotely
#' > # suppressMessages(library(remoter, quietly = TRUE))
#' > # client()
#' > remoter::client("192.168.56.101")
#'
#' remoter> rhelp("plot")
#' remoter> ?rhelp
#'
#' remoter> help("par")
#'
#' remoter> q()
#' >
#' }
#' 
#' @rdname rhelp 
#' @name rhelp
NULL

#' @export
rhelp <- function(topic, package = NULL, lib.loc = NULL,
                  verbose = getOption("verbose"),
                  try.all.packages = getOption("help.try.all.packages"),
                  help_type = getOption("help_type"))
{
  set.status(need_auto_rhelp_on, TRUE)
  ret <- utils::help(topic, lib.loc = lib.loc,
                     verbose = verbose, try.all.packages = try.all.packages,
                     help_type = help_type)
  Rd <- print.rhelp_files_with_topic(ret)

  ### Visible is necessary because of retmoter_server_eval().
  return(Rd)
}



#' @rdname rhelp
#' @export
help <- rhelp



auto_rhelp_on_local <- function(Rd)
{
  temp <- tempfile("Rtxt")
  cat(Rd, file = temp, sep = "\n")
  file.show(temp, title = "R Help", delete.file = TRUE, encoding = "UTF-8")

  ### Directly cast to terminal if igetOption("pager") is not right.
  # cat(Rd, sep = "\n")

  invisible()
}



### Hijack utils:::print.help_files_with_topic()
print.rhelp_files_with_topic <- function(x)
{
  ### Only support text type
  attr(x, "type") <- "text"
  paths <- as.character(x)
  topic <- attr(x, "topic")

  if (!length(paths))
  {
    ret <- c(gettextf("No documentation for %s in specified packages and libraries:", 
                      sQuote(topic)))
    return(invisible(ret))
  }

  if (attr(x, "tried_all_packages"))
  {
    paths <- unique(dirname(dirname(paths)))
    msg <- gettextf("Help for topic %s is not in any loaded package but can be found in the following packages:",
                    sQuote(topic))
    ret <- c(strwrap(msg), "",
             paste(" ", formatDL(c(gettext("Package"), basename(paths)),
                                 c(gettext("Library"), dirname(paths)), 
                                 indent = 22)),
             "Specify the package name to rhelp(topic, package = ...)")
  }
  else
  {
    ### Check multiple topics and disable menu selection.
    if (length(paths) > 1L)
    {
      msg <- gettextf("Help on topic %s was found in the following packages:",
                      sQuote(topic))
      paths <- dirname(dirname(paths))
      txt <- formatDL(c("Package", basename(paths)), c("Library", 
                      dirname(paths)), indent = 22L)
      ret <- c(strwrap(msg), "", paste(" ", txt), "",
               "Specify the package name to rhelp(topic, package = ...)")
    }
    else
    {
      file <- paths
      pkgname <- basename(dirname(dirname(file)))
      .getHelpFile <- eval(parse(text = "utils:::.getHelpFile"))
      temp <- Rd2txt(.getHelpFile(file), out = tempfile("Rtxt"), 
                     package = pkgname, outputEncoding = "UTF-8")
      ret <- readLines(temp, warn = FALSE, encoding = "UTF-8")
      file.remove(temp)
    }
  }

  invisible(ret)
}
