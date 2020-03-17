#' Repeat multiple elements multiple times
#'
#' @param vecx Vector of elements
#' @param vecy Vector of times
#'
#' @export

mrep <- function(vecx, vecy) {
  do.call(c, mapply(function(x, y) rep(x, y), vecx, vecy, SIMPLIFY = FALSE))
}
