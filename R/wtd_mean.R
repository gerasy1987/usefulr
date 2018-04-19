#' Compute weighted mean.
#'
#' @param x A numeric vector.
#' @param weights A numeric vector of weights.
#' @param na.rm Set to FALSE to suppress checking for NAs
#' @return Returns scalar vector of length one.
#' @examples
#' \dontrun{set.seed(1)
#'          x <- runif(500)
#'          wts <- sample(1:6, 500, TRUE)
#'          mn <- wtd_mean(x = x, weights = wts)}
#'
#' @export

wtd_mean <- function (x, weights = NULL, na.rm = TRUE) {
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  return(sum(weights * x)/sum(weights))
}


#' Compute weighted standard deviation.
#'
#' @param x A numeric vector.
#' @param weights A numeric vector of weights.
#' @param na.rm Set to FALSE to suppress checking for NAs
#' @return Returns scalar vector of length one.
#' @examples
#' \dontrun{set.seed(1)
#'          x <- runif(500)
#'          wts <- sample(1:6, 500, TRUE)
#'          mn <- wtd_mean(x = x, weights = wts)
#'          sd <- wtd_sd(x = x, weights = wts)}
#'
#' @export

wtd_sd <- function (x, weights = NULL, na.rm = TRUE) {
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(sd(x, na.rm = na.rm))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  sw <- sum(weights)

  if (sw <= 1) warning("only one effective observation; variance estimate undefined")

  xbar <- sum(weights * x)/sw
  return(sqrt(sum(weights * ((x - xbar)^2))/(sw - 1)))
}
