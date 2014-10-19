### Please reserver room for terminal.

pbd.server <- function(method = c("shiny", "terminal"),
    mpicmd = "mpiexec", np = 2, hostfile = NULL, Rcmd = "Rscript",
    # preload = NULL,
    package = "pbdinline"){
  mpicmd <- paste(mpicmd, " -np ", np, sep = "")
  if(! is.null(hostfile)){
    mpicmd <- paste(mpicmd, " --hostfile ", hostfile, sep = "")
  }
  mpicmd <- paste(mpicmd, " ", Rcmd, sep = "")

  ### I may need this if "mpiexec" or "Rscript" is not in PATH.
  # if(!is.null(preload)){
  #   mpicmd <- paste("source ", preload, "; ", mpicmd, sep = "")
  # }

  if(method[1] == "shiny"){
    file.name <- "server.r"
    file.path <- tools::file_path_as_absolute(
                   system.file(file.name, package = package))
    mpicmd <- paste(mpicmd, " ", file.path, sep = "")
    ret <- system(mpicmd, intern = TRUE, wait = FALSE)

    ### I suppose this should return pid that I can kill it later.
    ### Or, with url which I can input to pbd_client in current R session.
  } else if(method[1] == "terminal"){
    stop("method is TBD.")
  } else{
    stop("method is not found.")
  }

  invisible()
} # End of pbd.server().


pbd.client <- function(URL, method = c("shiny", "terminal")){
  if(method[1] == "shiny"){
    browseURL(URL)
  } else if(method[1] == "terminal"){
    # Open an R (current) session to the return value of pbd_server.
  } else{
    stop("method is not found.")
  }

  invisible()
} # End of pbd.client().

