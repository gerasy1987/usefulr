#' Multiple pattern replacement.
#'
#' @param pattern Character vector containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible.
#' @param replacement A replacement for matched pattern in sub and gsub. Coerced to character if possible.
#' @param x A character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param ... Other parameters passed to \code{gsub()}
#' @return Numeric vector.
#' @examples
#' \dontrun{
#'    mgsub(pattern = c("_", " ["),
#'          replacement = c("\\_", paste("", .latex_spacing, "[")),
#'          x = x,
#'          fixed = TRUE)
#'          }
#'
#' @export

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern) != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
