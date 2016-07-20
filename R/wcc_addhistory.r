### Add to history() and avoid repeatedly appending suffix.
addhistory <- function(read)
{
  tmp <- as.character(read)
  suffix <- paste0(" # ", getval(prompt))
  if (!grepl(x = tmp, pattern = paste0(suffix, "$"), perl = TRUE))
    tmp <- paste0(tmp, suffix)

  ### TODO: need to check for OS X, X11, ... etc.
  ###       check if hangs, then check if up/down recalls work.
  if (isWindows() && isRStudio())
  {
    ### This adds history to native R but is insufficient for Rtudio IDE.
    # .rs.registerReplaceHook("timestamp", "utils", function(original, ...)
    # {
    #   invisible(.Call("rs_timestamp", "bb"))
    # })
    # timestamp()
  }
  else
    utils::timestamp(stamp = tmp, prefix = "", suffix = "", quiet = TRUE)

  invisible()
}
