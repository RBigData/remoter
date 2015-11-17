# remoter

* **Version:** 0.1-1
* **Status:** [![Build Status](https://travis-ci.org/wrathematics/remoter.png)](https://travis-ci.org/remoter/ngram)
* **License:** [![License](http://img.shields.io/badge/license-BSD%202--Clause-orange.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** Drew Schmidt and Wei-Chen Chen


Control a remote R session from your local R session.  Uses 
[**pbdZMQ**](https://github.com/snoweye/pbdZMQ)
to handle the communication and networking. The custom REPL is 
based off of [**pbdCS**](https://github.com/wrathematics/pbdCS).



## Usage

#### Basics

For now, you have to manually set up the server on your remote
instance.  To do this:

1. ssh to your remote (you only need to do this once!)
2. Start a tmux session
3. Start R and run `remoter::server()` (see `?server` for additional options).  Or even better, run `Rscript -e remoter::server()` so the server dies if something goes wrong.
5. Detach your tmux session and log out.

Once that's ready, you can connect to your remote via:

```r
remoter::client("my.remote.address")
```

So for example, say I have set up a server (as described above)
on EC2 with address `"ec2-1-2-3-4.compute-1.amazonaws.com"`,
listening on port `56789`. Then I would run:

```r
remoter::client("ec2-1-2-3-4.compute-1.amazonaws.com", port=56789)
```

Alternatively, you can set up the server on your local machine
just to see how it works.  In that case, you can use the 
remote address `"localhost"`.
If you're using an actually remote machine though, make sure
you forward your port on the server-side before trying to
connect to it.


#### Utility Functions

There are a few utility functions available that have to do with
handling execution of things locally or moving data between client
and server.

By default, all commands entered inside of the client are executed
on the server.  If you need to do some things in the local R session,
you can kill the client and just reconnect when you're ready.
Alternatively, you can use the `lsc()`, `rmc()`, and
`evalc()` functions.  These are client versions of `ls()`, 
`rm()`, and `eval()`.  

For moving data between client and server, there are the
`s2c()` and `c2s()` commands which transfer from server to
client and from client to server, respectively.



## Installation

You should probably first install the development version of
pbdZMQ, on which this package relies (there is a version on CRAN,
but it is out of date):

```r
devtools::install_github("snoweye/pbdZMQ")
```

To install remoter:

```r
devtools::install_github("wrathematics/remoter")
```


## Problems, Bugs, and Other Maladies

The package should basically be useable, but there are some issues you might want to be aware of.

**Problem**: I lost my internet connection and the client is no longer sending messages.

**Solution**: Just `Ctrl+c` and re-run the `remoter::client()` call and you should be good to go.  If you set up the server as I described, then it is persistent (until you kill it with `q()`).  You can therefore also have multiple clients connect to the same server, and they will share the same data.  I actually kind of like this behavior, but I'm not married to it and I could probably be convinced to change it.



**Problem**: Can't use up/down arrow when using the client.

**Explanation**: That's because the client is just some R code sitting on top of the R REPL.  This shouldn't be a problem if you're using an IDE like RStudio or the like.



**Problem**: There's no security!

**Explanation**: There's currently an idiot's (i.e. mine) version of a password system available.  This can be made more sophisticated, and probably will be in the near future, probably using the new package **sodium**.  As for encrypting all communications, I'm not sure how important that is really, but there's a possibility using CurveZMQ.


**Problem**: Something else is wrong!

**Explanation**: I'm not surprised.  Please be so kind as to [file an issue](https://github.com/wrathematics/remoter/issues) describing the problem.



## Acknowledgements

Almost the entirety of the source code for this package comes from a modification of the **pbdCS** package, the development for which was supported by the project *Harnessing Scalable Libraries for Statistical Computing on Modern Architectures and Bringing Statistics to Large Scale Computing* funded by the National Science Foundation Division of Mathematical Sciences under Grant No. 1418195.

Any  opinions,  findings,  and  conclusions  or  recommendations expressed  in  this  material  are those  of  the  authors  and  do  not necessarily  reflect  the  views  of  the  National  Science Foundation.
