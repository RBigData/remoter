### TODO FIXME integrate with remoter_sanitize()
remoter_eval_filter_server <- function(msg)
{
  if (all(grepl(x=msg, pattern="^(\\s+)?library\\(", perl=TRUE)))
  {
    msg <- paste0("
      tmp <- file(tempfile())
      sink(tmp, append=TRUE)
      sink(tmp, append=TRUE, type='message')\n", 
      msg, "\n
      sink()
      sink(type='message')
      cat(paste(readLines(tmp), collapse='\n'))
      unlink(tmp)
    ")
  }
  
  msg
}
