#' Summarize simulations
#'
#' Summarize simulations down to the time point at which a statistic passes a certain threhold (assuming monotonous increase).
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param sname Column name of the statistic to summarize
#' @param tname Time column
#' @param theta The threshold that needs to be passed
#' @param colname Optional name of the summary column
#'
#' @export

# Time at which a threshold is passed for a statistic (assuming monotonous increase)
summ_threshold <- function(
  d,
  sname = "x",
  tname = "t",
  theta = 0.9,
  colname = NULL
) {

  d <- d %>%
    group_by(id) %>%
    summarize(new = get(tname)[min(which(get(sname) >= theta))])

  if (!is.null(colname)) colnames(d)[colnames(d) == "summary"] <- colname

  return(d)

}
