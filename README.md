# pbdinline

This is a very simple R package that aids in using R and pbdR on
supercomputers.  It contains utilities for running pbdR scripts, as
well as allocating jobs with pbs schedulers.

This assumes you have MPI and the pbdR utilities installed, but wish
to control batch execution of pbdR scripts from R instead of the 
command line.



## Installation

The easiest way to install the package is using the devtools package:

```r
library(devtools)
install_github("wrathematics/pbdinline")
```



## Usage

See the demos/ section for example usage.
