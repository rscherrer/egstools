#' Combine datasets
#'
#' Combine multiple data frames and add extra columns to identify them.
#'
#' @param d A list of data frames to combine. Numbers of columns should match.
#' @param levs Optional list representing columns to add, where each element contains the levels to assign to each data frame.
#'
#' @export

combine_data <- function(d, levs = NULL) {

  if (!all(sapply(levs, length) == length(d))) stop("There should be as many levels as there are data frames to combine")

  n <- sapply(d, nrow)

  # Combine data frames
  combined <- do.call(rbind, d)

  # Append extra information if needed
  if (!is.null(levs)) {
    append <- do.call(cbind, lapply(levs, mrep, n))
    combined <- data.frame(cbind(combined, append))
  }

  return (combined)

}
