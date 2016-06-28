library(remoter)

# TODO is this acceptable on CRAN?
system("Rscript -e 'remoter::server()'", wait=FALSE)

script <- "
1+1

1+
2

shutdown()
"

test <- capture.output(
  batch(script=script)
)

# cat(paste0(ret, collapse="\n"))
# cat("\n")

truth <- c("", "[1] 2 ", "[1] 3 ")

stopifnot(all.equal(truth, test))
