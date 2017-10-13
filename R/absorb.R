#' Function to pass to lfe::getfe() function to return intercept as in arge function in Stata
#'
#' @param gamma A numeric vector of FE estimates from lfe::felm().
#' @param addnames A character vector of FE level names.
#' @param .FE A numeric vector of FE from the data.
#' @return Returns scalar vector of length one.
#' @examples
#' \dontrun{icpt <- unname(
#'                    plyr::name_rows(
#'                    lfe::getfe(fit, ef = absorb, se = T, bN = 1000)
#'                    )
#'                  )}
#'
#' @export

absorb <- function(gamma, addnames, .FE){
  ws <- table(.FE, useNA = 'no')
  icpt <- wtd_mean(gamma, weights = ws)  # first level of f1
  result <- c(icpt)
  if(addnames) {
    names(result) <- "intercept"
    attr(result, "extra") <- list(fe = factor("icpt"),
                                  obs = factor(length(.FE)))
  }
  result
}
