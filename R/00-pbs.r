### TODO error checking, better system independence, extra ???

setClass("bigmachine", 
  representation(
    corename="character",
    corespernode="numeric" #, ???
  )
)

bigmachine.new <- function(corename, corespernode)
{
  corespernode <- as.integer(corespernode)
  new("bigmachine", corename=corename, corespernode=corespernode)
}



setClass("pbs", 
  representation(
    name="character",
    account="character",
    shell="character",
    interactive="logical",
    walltime="character",
    ncores="numeric"
  ),
  prototype(
    name="CHANGEME",
    account="CHANGEME",
    shell="/bin/bash",
    interactive=TRUE,
    walltime="24:00:00",
    ncores=1
  )
)

pbs.new <- function(name, account, shell, interactive, walltime, ncores)
{
  job <- new("pbs")
  
  if (!missing(name)) job@name <- name
  if (!missing(account)) job@account <- account
  if (!missing(shell)) job@shell <- shell
  if (!missing(interactive)) job@interactive <- interactive
  if (!missing(walltime)) job@walltime <- walltime
  if (!missing(ncores)) job@ncores <- ncores
  
  return(job)
}



###########################################################

qsub <- function(job, machine, ncores, body) UseMethod("qsub")

qsub.pbs <- function(job, machine, ncores, body)
{
  if (class(job) != "pbsjob") stop("FIXME")
  
  script <- paste0(
    "#PBS -S ", job@shell, "\n",
    "#PBS -A ", jobaccount, "\n",
    if (job@interactive) "#PBS -I\n",
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


