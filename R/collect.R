#' Collect simulation data
#'
#' Gathers simulation data through time from multiple simulations.
#'
#' @param dir Where the simulation folders are
#' @param xfile Filename for the first dimension
#' @param yfile Filename for the second dimension
#' @param zfile Filename for the third dimension
#' @param parnames Optional names of the parameters to read and append
#' @param parfile Name of the file where to read parameter values. Used only if parameter names are provided.
#'
#' @export

read <- function(filename) {

  # Number of values to read
  n <- file.info(filename)$size / 8

  file <- file(filename, "rb")
  readBin(file, numeric(), n)

}

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

collect <- function(
  dir = ".",
  xfile = "EI.dat",
  yfile = "SI.dat",
  zfile = "RI.dat",
  parnames = c("hsymmetry", "ecosel", "tsave"),
  parfile = "paramlog.txt"
) {

  library(assertthat)

  # List the simulation folders
  folders <- list.files(dir, full.names = TRUE)
  folders <- folders[grep("sim_", folders)]

  # Loop through simulations
  d <- lapply(folders, function(folder) {

    xfile <- paste0(folder, '/', xfile)
    yfile <- paste0(folder, '/', yfile)
    zfile <- paste0(folder, '/', zfile)

    # Read the three variables of interest
    x <- read(xfile)
    y <- read(yfile)
    z <- read(zfile)

    assert_that(all(length(x) == length(y), length(y) == length(z)))

    t <- seq_along(x) - 1
    id <- rep(folder, length(x))

    # Put them in a data frame
    d <- data.frame(x, y, z, t, id)

    # Read parameters if necessary
    if (length(parnames) > 0) {
      parvalues <- read_params(parnames, dir = folder)
      parvalues <- as.data.frame(lapply(parvalues, rep, nrow(d)))
      names(parvalues) <- parnames
      d <- cbind(d, parvalues)
    }

    return(d)

  })

  d <- do.call(rbind, d)
  d$id <- as.factor(d$id)

  return(d)

}
