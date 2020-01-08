#' Summarize simulations
#'
#' Summarize simulations down to the time point at which a statistic passes a certain threhold (assuming monotonous increase).
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param sname Column name of the statistic to summarize
#' @param tname Time column
#' @param theta The threshold that needs to be passed
#' @param colname Optional name of the summary column
#' @param keep Optional names of columns to keep in the summary data frame
#'
#' @export

# Time at which a threshold is passed for a statistic (assuming monotonous increase)
summ_threshold <- function(
  d,
  sname = "x",
  tname = "t",
  theta = 0.9,
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

  d <- d %>%
    group_by(id) %>%
    summarize(new = get(tname)[min(which(get(sname) >= theta))])

  if (!is.null(colname)) colnames(d)[colnames(d) == "summary"] <- colname
  if (!is.null(keep)) d <- cbind(d, meta)

  return(d)

}
