#' Read simulation parameters
#'
#' Reads the values of parameters of interest for a given simulation.
#'
#' @param parnames The names of the parameters to read
#' @param parfile The name of the parameter file
#' @param dir Where the parameter file is located. Defaults to the current working directory.
#'
#' @export

read_params <- function(parnames, parfile = "paramlog.txt", dir = ".") {

  # Read the parameter log file
  parfile <- paste0(dir, "/", parfile)
  params <- read.delim(parfile, header = FALSE)
  params <- params[, 1]

  # Extract values of the parameters of interest
  parvalues <- sapply(parnames, function(parname) {

    i <- grep(parname, params)
    if (length(i) == 0) stop(paste("Unknown parameter", parname))
    param <- as.character(params[i])
    param <- gsub(paste0(parname, ' '), '', param)
    param <- gsub(' $', '', param)

  })

  return(as.numeric(parvalues))

}
