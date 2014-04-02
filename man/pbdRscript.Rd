\name{pbdRscript}
\alias{pbdRscript}


\title{ pbdRscript }
\description{
  Simple tool for executing pbdR batch jobs from inside R.
}
\usage{
  pbdRscript(body, cores=1, intern=FALSE, auto=TRUE, auto.dmat=FALSE)
}
\arguments{
  \item{body}{character; the pbdR script to be evaluated.}
  \item{cores}{numeric}
  \item{intern}{logical; determines if the output of the evaluation of
  the script should be saved as an R character vector.}
  \item{auto}{logical; determines if the script should automatically
  load the pbdMPI package, and call init() (at the beginning) and 
  finalize() (at the end).}
  \item{auto.dmat}{logical; determines if the script should automatically
  load the pbdDMAT package and call init.grid(). Automatically sets
  auto=TRUE.}
}
\details{
  This function is a simple wrapper around the system() command.  As such,
  data is not shared between the calling R process and the batch processes
  which execute the 'body' source.
}
