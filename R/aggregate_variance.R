#' Aggregate replicate simulations
#'
#' Aggregate a statistic down to its variance over replicates within values of one or two parameters.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param xname Column name of the first grouping parameter
#' @param yname Column name of the second grouping parameter (set to NULL if only one grouping parameter)
#' @param zname Column name of the statistic to aggregate
#' @param colname Optional name of the aggregated statistic column
#'
#' @export

aggregate_variance <- function(
  d,
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

  d <- d %>% summarize(Z = var(get(zname)))

  colnames(d)[1] <- xname
  if (!is.null(yname)) colnames(d)[2] <- yname

  if (!is.null(colname)) colnames(d)[colnames(d) == "Z"] <- colname

  return(d)

}
