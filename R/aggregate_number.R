#' Aggregate replicate simulations
#'
#' Aggregate a statistic over replicates by counting how many have values above a threshold, within values of one or two parameters.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param theta The threshold to pass
#' @param xname Column name of the first grouping parameter
#' @param yname Column name of the second grouping parameter (set to NULL if only one grouping parameter)
#' @param zname Column name of the statistic to aggregate
#' @param colname Optional name of the aggregated statistic column
#' @param keep Optional names of columns to keep in the aggregated data frame
#'
#' @export

aggregate_number <- function(
  d,
  theta = 0.9,
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "summary",
  colname = NULL,
  keep = NULL
) {

  d <- d %>% group_by(get(xname))
  if (!is.null(yname)) d <- d %>% group_by(get(yname), add = TRUE)
  if (!is.null(keep)) {
    for (i in seq_along(keep)) {
      d <- d %>% group_by(get(keep[i]), add = TRUE)
    }
  }

  d <- d %>% summarize(Z = length(which(get(zname) >= theta)))

  ycol <- ifelse(is.null(yname), 1, 2)
  keepcols <- ycol + seq_along(keep)

  colnames(d)[1] <- xname
  if (!is.null(yname)) colnames(d)[ycol] <- yname
  if (!is.null(keep)) colnames(d)[keepcols] <- keep

  if (!is.null(colname)) colnames(d)[colnames(d) == "Z"] <- colname

  return(d)

}
