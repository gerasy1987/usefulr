
#' OLS Estimator
#'
#' @param frame_df DESCRIPTION.
#' @param fit_formula DESCRIPTION.
#' @param IPW Inverse probability weights specified as character vector.
#' @param robust Logical. Whether to report heteroskedastic robust standard errors. Implemented only for linear models for now.
#' @param col_names DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @importFrom lfe felm
#' @importFrom multiwayvcov cluster.vcov
#' @importFrom lmtest coeftest
#' @export
estimate_lm <- function(frame_df,
                        fit_formula,
                        IPW,
                        robust,
                        col_names,
                        ...) {

  requireNamespace("lfe", quietly = TRUE)

  if (is.null(IPW)) {
    fit <-
      suppressWarnings(
        lfe::felm(formula = fit_formula,
                  data = frame_df)
      )
  } else {
    fit <-
      suppressWarnings(
        lfe::felm(formula = fit_formula,
                  data = frame_df,
                  weights = unlist(frame_df[, IPW]))
      )
  }

  estout <-
    tibble::as_tibble(summary(fit, robust = robust)$coefficients, rownames = "variable")[,c(1:3,5)]
  colnames(estout) <- col_names

  return(estout)

}

