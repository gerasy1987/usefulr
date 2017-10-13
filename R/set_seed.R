#' Wrapper for set.seed function in R.
#'
#' @param .seed The number for the random number generation.
#' @param .parallel Logical. Whether to set seed for parallel computation.
#' @return Nothing.
#' @examples
#' \dontrun{
#' set_seed(.seed = 12345,
#'          .parallel = FALSE)
#'          }
#'
#' @export

set_seed <- function(.seed = 12345, .parallel = FALSE) {
  # required packages
  requireNamespace("mosaic", quietly = TRUE)

  if (.parallel) mosaic::set.rseed(seed = .seed)
  else set.seed(seed = .seed)
}
