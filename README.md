# remoter

* **Version:** 0.4-0
* **License:** [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** Drew Schmidt and Wei-Chen Chen
* **Project home**: https://github.com/RBigData/remoter
* **Bug reports**: https://github.com/RBigData/remoter/issues


Control a remote R session from your local R session.  The package uses [**pbdZMQ**](https://github.com/snoweye/pbdZMQ) to handle the communication and networking.  Encryption is supported if the **sodium** package is (optionally) installed.  Details below.





## Installation

You can install the stable version from CRAN using the usual `install.packages()`:

```r
install.packages("remoter")
```

In order to be able to create and connect to secure servers, you need to also install the **sodium** package.  The use of **sodium** is optional because it is a non-trivial systems dependency, but it is highly recommended.  You can install it manually with a call to `install.packages("sodium")` or by installing **remoter** via:

```r
install.packages("remoter", dependencies=TRUE)
```

The development version is maintained on GitHub, and can easily be installed by any of the packages that offer installations from GitHub:

```r
### Pick your preference
devtools::install_github("RBigData/remoter")
ghit::install_github("RBigData/remoter")
remotes::install_github("RBigData/remoter")
```

To simplify installations on cloud systems, we also have a [Docker container](https://github.com/RBigData/docker/tree/master/pbdr-remoter) available.




## Usage

For setting up a local server, you can do:

```r
remoter::server()
```

And connect to it interactively via:

```r
remoter::client()
```

There is also the option to pipe commands to the server in batch using the `batch()` function:

```r
### Passing an R script file
remoter::batch(file="my_rscript_file.r")
### Passing in a script manually
remoter::batch(script="1+1")
```

For more details, including working with remote machines, see the package vignette.





## Acknowledgements

Work for the **remoter** package was supported in part by the project *Harnessing Scalable Libraries for Statistical Computing on Modern Architectures and Bringing Statistics to Large Scale Computing* funded by the National Science Foundation Division of Mathematical Sciences under Grant No. 1418195.

Any  opinions,  findings,  and  conclusions  or  recommendations expressed  in  this  material  are those  of  the  authors  and  do  not necessarily  reflect  the  views  of  the  National  Science Foundation.
