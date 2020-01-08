#' Summarize simulations
#'
#' Summarize simulations down to the average value of a statistic between two time points.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param sname Column name of the statistic to summarize
#' @param tname Time column
#' @param trange The two values between which to average (defaults to the entire range)
#' @param colname Optional name of the summary column
#' @param keep Optional names of columns to keep in the summary data frame
#'
#' @export

# Average value of a statistic between two timesteps (defaults to whole time period)
summ_average <- function(
  d,
  sname = "x",
  tname = "t",
  trange = NULL,
  colname = NULL,
  keep = NULL
) {

  if (!is.null(trange)) {
    assert_that(length(trange) == 2)
    assert_that(trange[1] <= trange[2])
  }

  if (!is.null(keep)) {
    meta <- lapply(keep, function(curr_column) {
      with(d, tapply(get(curr_column), id, "[", 1))
    })
    meta <- as.data.frame(do.call(cbind, meta))
    colnames(meta) <- keep
  }

  d <- d %>% group_by(id)

  if (is.null(trange)) {
    d <- d %>% summarize(summary = mean(get(sname)))
  } else {
    d <- d %>% summarize(summary = mean(get(sname)[get(tname) > trange[1] & get(tname) < trange[2]]))
  }

  if (!is.null(colname)) colnames(d)[colnames(d) == "summary"] <- colname
  if (!is.null(keep)) d <- cbind(d, meta)

  return(d)

}
