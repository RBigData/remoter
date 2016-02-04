# remoter

* **Version:** 0.2-0
* **URL**: https://github.com/wrathematics/remoter
* **Status:** [![Build Status](https://travis-ci.org/wrathematics/remoter.png)](https://travis-ci.org/wrathematics/remoter)
* **License:** [![License](http://img.shields.io/badge/license-BSD%202--Clause-orange.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** Drew Schmidt and Wei-Chen Chen


Control a remote R session from your local R session.  Uses 
[**pbdZMQ**](https://github.com/snoweye/pbdZMQ)
to handle the communication and networking. The custom REPL is 
based off of [**pbdCS**](https://github.com/wrathematics/pbdCS).



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

The development version is maintained on GitHub.  You can install this version using the `devtools` package:

```r
devtools::install_github("wrathematics/remoter")
```



## Acknowledgements

Almost the entirety of the source code for this package comes from a modification of the **pbdCS** package, the development for which was supported by the project *Harnessing Scalable Libraries for Statistical Computing on Modern Architectures and Bringing Statistics to Large Scale Computing* funded by the National Science Foundation Division of Mathematical Sciences under Grant No. 1418195.

Any  opinions,  findings,  and  conclusions  or  recommendations expressed  in  this  material  are those  of  the  authors  and  do  not necessarily  reflect  the  views  of  the  National  Science Foundation.
