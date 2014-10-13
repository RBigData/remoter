qsub <- function(job, machine, ncores, body) UseMethod("qsub")

qsub.pbs <- function(job, machine=.MACHINE, body)
{
  if (class(job) != "pbsjob") stop("FIXME")
  
  script <- paste0(
    "#PBS -S ", job@shell, "\n",
    "#PBS -A ", jobaccount, "\n",
    if (job@isInteractive) "#PBS -I\n",
    "#PBS -l", machine@corename, "=", job@ncores, ",walltime=", job@walltime,, "\n\n",
    body
  )
  
  tmp <- tempfile()
  writeLines(text=script, con=tmp)
  
  jobnum <- system(paste("qsub", f), intern=TRUE)
  jobnum <- as.numeric(sub(x=jobnum, pattern="[.].*", replacement=""))
  
  unlink(tmp)
  
  return( jobnum )
}



qdel <- function(jobnum)
{
  system(paste("qdel", jobnum), intern=TRUE)
}



qstat <- function(jobnum)
{
  if (missing(jobnum))
    return(system("qstat", intern=TRUE))
  
  check <- system(paste("qstat", jobnum), intern=TRUE)
  
  names <- gsub(x=check[1L], pattern="  ", replacement="")
  names <- unlist(strsplit(x=names, split=" "))
  names <- c("Job.id", names[-(1:2)])
  
  job <- gsub(x=check[length(check)], pattern="  ", replacement="")
  job <- unlist(strsplit(x=job, split=" "))
  
  names(job) <- names
  
  return( job )
}


