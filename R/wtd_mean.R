#' Compute weighted mean.
#'
#' @param x A numeric vector.
#' @param weights A numeric vector of weights.
#' @param normwt Specify normwt=TRUE to make weights sum to length(x) after deletion of NAs.
#' @param na.rm Set to FALSE to suppress checking for NAs
#' @return Returns scalar vector of length one.
#' @examples
#' \dontrun{set.seed(1)
#'          x <- runif(500)
#'          wts <- sample(1:6, 500, TRUE)
#'          mn <- wtd.mean(x = x, weights = wts)}
#'
#' @export

wtd_mean <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  return(sum(weights * x)/sum(weights))
}
