# remoter

Control a remote R session.  Uses pbdZMQ to handle communication.
The custom REPL is based off of pbdCS.


## Usage

For now, you have to manually set up the server on your remote
instance.  To do this:

1. ssh to your remote (you only need to do this once!)
2. Start a tmux session
3. Start R
4. `remoter::server()`
5. Detach your tmux session and log out.

Once that's ready, you can connect to your remote via:

```r
remoter::client("my.remote.address")
```

Make sure you forward your port on the server-side before trying to
connect to it.


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
