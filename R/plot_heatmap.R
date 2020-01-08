#' Plot heatmap of a statistic across parameter space
#'
#' Compute a summary statistic for each simulation, then aggregates replicate simutions per combination of parameters and plot the result.la
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param labs Labels for each axis
#' @param xname Column name of the x-axis
#' @param yname Column name of the y-axis
#' @param zname Column name of the variable to summarize
#' @param tname Time column
#' @param summary Method used to summarize simulations. Either of "value" (picks a value at a given time point), "average" (between two time points) or "threshold" (the time at which a threshold is passed, assuming monotonous increase).
#' @param tval Timepoint to pick a value at (defaults to last generation). Used only if summary method is "value".
#' @param trange Timepoints between which to average (defaults to the entire range). Used only if summary method is "average".
#' @param theta Threshold whose passing time (assuming monotonous increase) is recorded. Used only if summary method is "threshold".
#' @param colname Optional name of the summary column
#' @param aggregate Method used to aggregate replicate simulations. Either of "average", "variance", "quantile" or "number" (the number of replicates above a certain threshold of the summary statistic).
#' @param prob The quantile used to aggregate replicates. Used only if the aggregation method is "quantile".
#' @param threshold The threshold used if the aggregation method is "number".
#' @param colors Optional low and high ends of the color gradient
#' @param splitvar Facet splitting variable
#' @param splitvar2 Second facet splitting variable
#'
#' @export

plot_heatmap <- function(
  d,
  labs = NULL,
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "z",
  tname = "t",
  summary = "value",
  tval = NULL,
  trange = NULL,
  theta = 0.9,
  colname = NULL,
  aggregate = "average",
  prob = 0.95,
  threshold = 0.9,
  colors = NULL,
  splitvar = NULL,
  splitvar2 = NULL
) {

  library(tidyverse)

  # Summarize simulations
  if (summary == "value") {
    dsum <- summ_value(d, zname, tname, tval, keep = c(xname, yname))
  } else if (summary == "average") {
    dsum <- summ_average(d, zname, tname, trange, keep = c(xname, yname))
  } else if (summary == "threshold") {
    dsum <- summ_threshold(d, zname, tname, theta, keep = c(xname, yname))
  } else stop("Wrong value for argument summary")

  # Aggregate replicates
  if (aggregate == "average") {
    dred <- aggregate_average(dsum, xname, yname)
  } else if (aggregate == "variance") {
    dred <- aggregate_variance(dsum, xname, yname)
  } else if (aggregate == "quantile") {
    dred <- aggregate_quantile(dsum, prob, xname, yname)
  } else if (aggregate == "number") {
    dred <- aggregate_number(dsum, theta, xname, yname)
  } else stop("Wrong value for argument aggregate")

  # Make the heatmap
  p <- ggplot(data = dred, aes(x = get(xname), y = get(yname), fill = Z)) +
    geom_tile()

  if (!is.null(colors)) p <- p + scale_color_gradient(low = colors[1], high = colors[2])

  if (!is.null(labs)) p <- p + xlab(labs[1]) + ylab(labs[2])

  if (!is.null(splitvar)) {
    if (!is.null(splitvar2)) {
      p <- p + facet_grid(get(splitvar) ~ get(splitvar2))
    } else {
      p <- p + facet_wrap(~get(splitvar))
    }
  }

  return(p)

}
