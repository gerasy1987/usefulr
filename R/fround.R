#' Take a numeric and return its rounded value with set number of decimal points.
#'
#' @param x Any numeric value.
#' @param digits Integer. Number of decimal points to report.
#' @return Numeric vector.
#' @examples
#' fround(x = 1/3,
#'        digits = 3)
#'
#' @export

fround <- function (x, digits) {
  format(round(x, digits), nsmall = digits)
}
