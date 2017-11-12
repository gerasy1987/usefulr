#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param ... DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
listn <- function(...) {
  input <- sapply(match.call(), deparse)[-1]
  if (is.null(names(input))) {
    names(input) <- rep("", times = length(input))
  }
  names(input) <- ifelse(names(input) == "", input, names(input))
  listWithName <- list(...)
  names(listWithName) <- names(input)
  return(listWithName)
}



#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param p.value DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
add_stars <- function(p.value) {
  return(
    ifelse(p.value <= 0.01, "***",
           ifelse(p.value <= 0.05, "**",
                  ifelse(p.value <= 0.1,"*",
                         ifelse(p.value <= 0.15, "+", ""))))
  )
}

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .matrix DESCRIPTION.
#' @param .names DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
make_df <- function(.matrix,
                    .names = c("estimate", "std.error", "p.value", "term")) {

  requireNamespace("dplyr", quietly = TRUE)

  df <- dplyr::as_data_frame(.matrix)
  df$term <- rownames(.matrix)
  names(df) <- .names
  return(df)
}
