### bigmachine

setMethod("corename", signature(x="bigmachine"),
  function(x) x@corename
)

setReplaceMethod("corename", signature(x="bigmachine"),
  function(x, value)
  {
    x@corename <- value
    return(x)
  }
)

setMethod("corespernode", signature(x="bigmachine"),
  function(x) x@corespernode
)

setReplaceMethod("corespernode", signature(x="bigmachine"),
  function(x, value)
  {
    x@corespernode <- value
    return(x)
  }
)



### pbs

setMethod("name", signature(x="pbs"),
  function(x) x@name
)

setReplaceMethod("name", signature(x="pbs"),
  function(x, value)
  {
    x@name <- value
    return(x)
  }
)

setMethod("account", signature(x="pbs"),
  function(x) x@account
)

setReplaceMethod("account", signature(x="pbs"),
  function(x, value)
  {
    x@account <- value
    return(x)
  }
)

setMethod("shell", signature(x="pbs"),
  function(x) x@shell
)

setReplaceMethod("shell", signature(x="pbs"),
  function(x, value)
  {
    x@shell <- value
    return(x)
  }
)

setMethod("isInteractive", signature(x="pbs"),
  function(x) x@isInteractive
)

setReplaceMethod("isInteractive", signature(x="pbs"),
  function(x, value)
  {
    x@isInteractive <- value
    return(x)
  }
)

setMethod("walltime", signature(x="pbs"),
  function(x) x@walltime
)

setReplaceMethod("walltime", signature(x="pbs"),
  function(x, value)
  {
    x@walltime <- value
    return(x)
  }
)

setMethod("ncores", signature(x="pbs"),
  function(x) x@ncores
)

setReplaceMethod("ncores", signature(x="pbs"),
  function(x, value)
  {
    x@ncores <- value
    return(x)
  }
)
