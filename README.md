# remoter

* **Version:** 0.3-2
* **URL**: https://github.com/RBigData/remoter
* **License:** [![License](http://img.shields.io/badge/license-BSD%202--Clause-orange.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** Drew Schmidt and Wei-Chen Chen


Control a remote R session from your local R session.  The packages uses [**pbdZMQ**](https://github.com/snoweye/pbdZMQ) to handle the communication and networking. The custom REPL is based off of [**pbdCS**](https://github.com/wrathematics/pbdCS).

Encryption is supported if the **sodium** package is installed.  Details below.



## Usage

For setting up a local server, you can do:

```r
remoter::server()
```

And connect to it via:

```r
remoter::client()
```

For more details, including working with remote machines, see the package vignette.



## Installation

You can install the stable version from CRAN using the usual `install.packages()`:

```r
install.packages("remoter")
```

In order to be able to create and connect to secure servers, you should also install the **sodium** package.  You can install it manually with a call to `install.packages("sodium")` or by installing **remoter** via:

```r
install.packages("remoter", dependencies=TRUE)
```


#### Development Version

The development version is maintained on GitHub, and can easily be installed by any of the packages that offer installations from GitHub:

```r
### Pick your preference
devtools::install_github("RBigData/remoter")
ghit::install_github("RBigData/remoter")
remotes::install_github("RBigData/remoter")
```



## Acknowledgements

Work for the **remoter** package is supported in part by the project *Harnessing Scalable Libraries for Statistical Computing on Modern Architectures and Bringing Statistics to Large Scale Computing* funded by the National Science Foundation Division of Mathematical Sciences under Grant No. 1418195.

Any  opinions,  findings,  and  conclusions  or  recommendations expressed  in  this  material  are those  of  the  authors  and  do  not necessarily  reflect  the  views  of  the  National  Science Foundation.
