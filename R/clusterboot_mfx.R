
#' Perform iteration of clustered marginal effects
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .model DESCRIPTION.
#' @param .cluster DESCRIPTION.
#' @param .variable DESCRIPTION.
#' @param .category DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#'
#' @importFrom margins margins
#' @importFrom MASS polr
#'
clusterboot_mfx <- function(.model,
                            .cluster,
                            .variable,
                            .category) {

  requireNamespace("margins", quietly = TRUE)
  requireNamespace("MASS", quietly = TRUE)

  .temp_df <- base::split(x = eval(.model$call$data), f = .cluster)
  .temp_df <- base::sample(.temp_df, size = length(.temp_df), replace = TRUE)
  .temp_df <- do.call("rbind", .temp_df)

  .model$call$data <- .temp_df
  .fit <- suppressWarnings(eval(.model$call))

  return(
    mean(margins::margins(.fit, variables = .variable, category = .category)[[paste0("dydx_", .variable)]])
  )
}
