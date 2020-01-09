#' Read data from file
#'
#' Reads binary data into a vector of values.
#'
#' @param filename The name of the file to read
#'
#' @export

read <- function(filename) {

  # Number of values to read
  n <- file.info(filename)$size / 8

  file <- file(filename, "rb")
  readBin(file, numeric(), n)

}
