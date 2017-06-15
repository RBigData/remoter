### Add to history() and avoid repeatedly appending suffix.
addhistory <- function(read)
{
  tmp <- as.character(read)
  suffix <- paste0(" # ", getval(prompt))
  if (!grepl(x = tmp, pattern = paste0(suffix, "$"), perl = TRUE))
    tmp <- paste0(tmp, suffix)

  utils::timestamp(stamp = tmp, prefix = "", suffix = "", quiet = TRUE)

  invisible()
}
