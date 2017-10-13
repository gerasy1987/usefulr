#' Take a numeric and return its rounded value with set number of decimal points as a character.
#'
#' @param x Any numeric value.
#' @param digits Integer. Number of decimal points to report.
#' @return Character vector.
#' @examples
#' pfround(x = 1/3,
#'         digits = 3)
#'
#' @export

pfround <- function (x, digits) {
  print(fround(x, digits), quote = FALSE)
}
