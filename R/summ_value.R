#' Summarize simulations
#'
#' Summarize simulations down to the value of a statistic picked at a given time.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param sname Column name of the statistic to summarize
#' @param tname Time column
#' @param tval Timepoint to pick (defaults to last generation)
#' @param colname Optional name of the summary column
#' @param keep Optional names of columns to keep in the summary data frame
#'
#' @export

# Value of a statistic at a given time (defaults to last time step)
summ_value <- function(
  d,
  sname = "x",
  tname = "t",
  tval = NULL,
  colname = NULL,
  keep = NULL
) {

  if (!is.null(keep)) {
    meta <- lapply(keep, function(curr_column) {
      with(d, tapply(get(curr_column), id, "[", 1))
    })
    meta <- as.data.frame(do.call(cbind, meta))
    colnames(meta) <- keep
  }

  d <- d %>% group_by(id)

  if (is.null(tval)) {
    d <- d %>% summarize(summary = get(sname)[n()])
  } else {
    d <- d %>% summarize(summary = get(sname)[get(tname) == tval])
  }

  if (!is.null(colname)) colnames(d)[colnames(d) == "summary"] <- colname
  if (!is.null(keep)) d <- cbind(d, meta)

  return(d)

}
