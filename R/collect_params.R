#' Collect simulation parameters
#'
#' Gathers simulation parameters for multiple simulations.
#'
#' @param parnames What parameters to extract
#' @param dir Where the simulation folders are
#' @param parfile Name of the parameter log file
#'
#' @export

collect_params <- function(parnames, dir = ".", parfile = "paramlog.txt") {

  # List the simulation folders
  folders <- list.files(dir, full.names = TRUE)
  folders <- folders[grep("sim_", folders)]

  # Loop through simulations
  d <- lapply(folders, function(folder) {

    parfile <- paste0(folder, '/', parfile)

    # Read the parameter log file
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

    # Put them in a data frame
    return(parvalues)

  })

  d <- do.call(rbind, d)
  d <- as.data.frame(d)

  return(d)

}
