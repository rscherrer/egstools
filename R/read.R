#' Read data
#'
#' Reads a binary data file into a numeric vector.
#'
#' @param filename Name of the data file (.dat format)
#'
#' @export

read <- function(filename) {

  # Number of values to read
  n <- file.info(filename)$size / 8

  file <- file(filename, "rb")
  readBin(file, numeric(), n)

}
