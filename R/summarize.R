#' Summarize simulations
#'
#' Summarize simulations through time by a single value.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param colvar What variable to color according to
#' @param labs Labels for each axis
#' @param xname Column name of the x-axis
#' @param yname Column name of the y-axis
#' @param tname Optional time column
#' @param splitvar Facet splitting variable
#' @param splitvar2 Second fact splitting variable
#'
#' @export

myfun <- function(x, tname = "t", tval = NULL) {

  # Extract a given time point
  if (is.null(tval)) tval <- c(x[nrow(x), tname])
  return(x[x[, tname] == tval, ])

}

myfun <- function(x, xnames = NULL, tname = "t", from = NULL, to = NULL) {

  if (is.null(from)) from <- c(x[1, tname])
  if (is.null(to)) to <- c(x[nrow(x), tname])
  if (is.null(xnames)) {
    xnames <- colnames(x)[sapply(x, class) == "numeric"]
    xnames <- xnames[xnames != tname]
  }

  # Extract the average between two time points
  x <- x[sapply(x[, tname], function(time) time >= from & time <= to), ]

  x %>% group_by(id) %>% summarize_if(is.numeric, mean) %>% ungroup()

}

summarize <- function(d) {

  library(tidyverse)

  d %>%
    group_by(id) %>%
    group_modify(~ myfun(.x))

  return(s)

}
