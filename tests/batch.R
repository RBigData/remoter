library(remoter)

system("${R_HOME}/bin${R_ARCH_BIN}/Rscript -e 'remoter::server()'", wait=FALSE)

script <- "
1+1

1+
2

shutdown()
"

test <- capture.output(
  batch(script=script)
)

truth <- c("", "[1] 2 ", "[1] 3 ")

stopifnot(all.equal(truth, test))
