getip <- function()
{
  if (!same.str(get.os(), "windows"))
  {
    ip <- .Call("pbdcs_getip")
  }
  else
  {
    ipconf <- system("ipconfig", intern = TRUE)
    ip <- gsub(".*? ([[:digit:]])", "\\1", ipconf[grep("IPv4", ipconf)])[1]
  }

  return(ip)
}
