#' Collect simulation parameters
#'
#' Gathers simulation parameters for multiple simulations.
#'
#' @param parnames What parameters to extract
#' @param dir Where the simulation folders are
#' @param parfile Name of the parameter log file
#' @param extclude Whether to exclude simulations that went extinct during burnin
#'
#' @export

collect_params <- function(parnames, dir = ".", parfile = "paramlog.txt", extclude = TRUE) {

  # List the simulation folders
  folders <- list.files(dir, full.names = TRUE)
  folders <- folders[grep("sim_", folders)]

  # Loop through simulations
  d <- lapply(folders, function(folder) {

    # Special treatment for extinctions
    if (extclude) {

      # Omit if the simulation went extinct
      files <- list.files(folder)
      logfile <- paste0(folder, '/', files[grep("slurm.*out", files)])
      log <- read.delim(logfile)
      isextinct <- FALSE
      i <- 1
      while (!isextinct & i <= nrow(log)) {
        line <- log[i,]
        match <- grep("extinct", line)
        if (length(match) > 0) isextinct <- TRUE
        i <- i + 1
      }

      if (isextinct) return(NULL)

    }

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
