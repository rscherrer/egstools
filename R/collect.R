#' Collect simulation data
#'
#' Gathers simulation data through time from multiple simulations.
#'
#' @param dir Where the simulation folders are
#' @param xfile Filename for the first dimension
#' @param yfile Filename for the second dimension
#' @param zfile Filename for the third dimension
#'
#' @export

collect <- function(dir = ".", xfile = "EI.dat", yfile = "SI.dat", zfile = "RI.dat") {

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

    id <- rep(folder, length(x))

    # Put them in a data frame
    return(data.frame(x, y, z, id))

  })

  d <- do.call(rbind, d)
  d$id <- as.factor(d$id)

  return(d)

}
