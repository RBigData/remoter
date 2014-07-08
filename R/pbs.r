### TODO error checking, better system independence

qsub <- function(name, account, walltime="24:00:00", body, shell="bash")
{
  if (shell == "bash")
    shell <- "/bin/bash"
  
  script <- paste(
    "#PBS -S ", shell, "\n",
    "#PBS -A ", account, "\n",
    "#PBS -l size=", ncores, ",walltime=", walltime,
    body,
    sep=""
  )
  
  tmp <- tempfile()
  writeLines(text=script, con=tmp)
  
  num <- system(paste("qsub", f), intern=TRUE)
  
  return( as.numeric(sub(x=num, pattern="[.].*", replacement="")) )
}



checkjob <- function(jobnum)
{
  check <- system(paste("qstat", jobnum), intern=TRUE)
  
  names <- gsub(x=check[1L], pattern="  ", replacement="")
  names <- unlist(strsplit(x=names, split=" "))
  names <- c("Job.id", names[-(1:2)])
  
  job <- gsub(x=check[length(check)], pattern="  ", replacement="")
  job <- unlist(strsplit(x=job, split=" "))
  
  names(job) <- names
  
  return( job )
}



