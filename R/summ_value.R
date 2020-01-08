#' Summarize simulations
#'
#' Summarize simulations down to the value of a statistic picked at a given time.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param sname Column name of the statistic to summarize
#' @param tname Time column
#' @param tval Timepoint to pick (defaults to last generation)
#' @param colname Optional name of the summary column
#'
#' @export

# Value of a statistic at a given time (defaults to last time step)
summ_value <- function(
  d,
  sname = "x",
  tname = "t",
  tval = NULL,
  colname = NULL
) {

  d <- d %>% group_by(id)

  if (is.null(tval)) {
    d <- d %>% summarize(summary = get(sname)[n()])
  } else {
    d <- d %>% summarize(summary = get(sname)[get(tname) == tval])
  }

  if (!is.null(colname)) colnames(d)[colnames(d) == "summary"] <- colname

  return(d)

}
