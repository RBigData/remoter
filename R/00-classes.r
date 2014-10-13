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
    isInteractive="logical",
    walltime="character",
    ncores="numeric"
  ),
  prototype(
    name="CHANGEME",
    account="CHANGEME",
    shell="/bin/bash",
    isInteractive=TRUE,
    walltime="24:00:00",
    ncores=1
  )
)

pbs.new <- function(name, account, shell, isInteractive, walltime, ncores)
{
  job <- new("pbs")
  
  if (!missing(name)) job@name <- name
  if (!missing(account)) job@account <- account
  if (!missing(shell)) job@shell <- shell
  if (!missing(isInteractive)) job@isInteractive <- isInteractive
  if (!missing(walltime)) job@walltime <- walltime
  if (!missing(ncores)) job@ncores <- ncores
  
  return(job)
}

