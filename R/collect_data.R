#' Collect simulation data
#'
#' Gathers simulation data through time from multiple simulations.
#'
#' @param dir Where the simulation folders are
#' @param files Filenames of the data to extract
#' @param parnames Optional names of the parameters to read and append
#' @param parfile Name of the file where to read parameter values. Used only if parameter names are provided.
#'
#' @note It is important to not have the string "sim_" in the path leading to where the simulation folders are. This is because the function to collect data looks for simulation folders using the pattern sim_ in the full path of those folders. If sim_ appears somewhere else in the path than in the name of the simulation folders, all files in the directory containing the simulation folders will be interpreted as simulation folders, even if they are not, e.g. python scripts.
#'
#' @export

collect_data <- function(
  dir = ".",
  files = c("EI.dat", "SI.dat", "RI.dat"),
  parnames = c("hsymmetry", "ecosel"),
  parfile = "paramlog.txt"
) {

  library(assertthat)

  # List the simulation folders
  folders <- list.files(dir, full.names = TRUE)
  folders <- folders[grep("sim_", folders)]

  # Loop through simulations
  d <- lapply(folders, function(folder) {

    files <- sapply(files, function(curr_file) paste0(folder, '/', curr_file))

    # Read the three variables of interest
    d <- lapply(files, read)

    assert_that(all(sapply(d, function(x) length(x) == length(d[[1]]))))

    # Put them in a data frame
    d <- as.data.frame(do.call(cbind, d))
    colnames(d) <- gsub("\\.dat", "", colnames(d))

    # Time steps and simulation ID
    t <- seq_len(nrow(d)) - 1
    id <- rep(folder, length(t))

    # Add metadata
    d <- data.frame(d, t = t, id = id)

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
