#' Aggregate replicate simulations
#'
#' Aggregate a statistic to a quantile over replicates within values of one or two parameters.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param prob Which quantile? (defaults to 0.95)
#' @param xname Column name of the first grouping parameter
#' @param yname Column name of the second grouping parameter (set to NULL if only one grouping parameter)
#' @param zname Column name of the statistic to aggregate
#' @param colname Optional name of the aggregated statistic column
#'
#' @export

aggregate_quantile <- function(
  d,
  prob = 0.95,
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "summary",
  colname = NULL
) {

  if (is.null(yname)) {
    d <- d %>% group_by(get(xname))
  } else {
    d <- d %>% group_by(get(xname), get(yname))
  }

  d <- d %>% summarize(Z = quantile(get(zname), prob = prob))

  colnames(d)[1] <- xname
  if (!is.null(yname)) colnames(d)[2] <- yname

  if (!is.null(colname)) colnames(d)[colnames(d) == "Z"] <- colname

  return(d)

}
