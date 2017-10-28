#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .model DESCRIPTION.
#' @param .cluster DESCRIPTION.
#' @param .adjust DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
vcov_cluster <- function(.model,
                         .cluster = NULL,
                         .adjust = NULL) {

  requireNamespace("sandwich", quietly = TRUE)

  ## .cluster specification
  if (is.null(.cluster)) .cluster <- attr(.model, "cluster")
  if (is.null(.cluster)) stop("no '.cluster' specification found.\n")

  .cluster <- factor(.cluster)

  ## estimating functions and dimensions
  ef <- sandwich::estfun(.model)
  n <- base::nrow(ef)
  k <- base::ncol(ef)

  if (n != length(.cluster))
    stop("length of '.cluster' does not match number of observations.\n")
  m <- length(levels(.cluster))

  ## aggregate estimating functions by .cluster and compute meat
  ef <- sapply(levels(.cluster), function(i) colSums(ef[.cluster == i, , drop = FALSE]))
  ef <- if (ncol(ef) > 1L) t(ef) else matrix(ef, ncol = 1L)
  mt <- crossprod(ef)/n

  ## bread
  br <- try(sandwich::bread(.model), silent = TRUE)
  if (inherits(br, "try-error")) br <- vcov(.model) * n

  ## put together sandwich
  vc <- 1/n * (br %*% mt %*% br)

  ## adjustment
  if (is.null(.adjust)) .adjust <- class(.model)[1L] == "lm"
  adj <- if (.adjust) m/(m - 1L) * (n - 1L)/(n - k) else m/(m - 1L)

  ## return
  return(adj * vc)
}
