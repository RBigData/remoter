if (.Platform$OS.type != "windows") {
  system("${R_HOME}/bin${R_ARCH_BIN}/Rscript -e \"remoter::server()\"", wait=FALSE)
} else {
  R.HOME <- Sys.getenv("R_HOME")
  R.ARCH.BIN <- Sys.getenv("R_ARCH_BIN")
  cmd <- paste(R.HOME, "/bin", R.ARCH.BIN, "/Rscript -e \"remoter::server()\"",
               sep = "")
  system(cmd, wait=FALSE)
}

script <- "
1+1

1+
2
shutdown()
"

test <- capture.output(remoter::batch(script = script))

truth <- c("", "[1] 2 ", "[1] 3 ")

stopifnot(all.equal(truth, test))
