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
#' @param p.value Numeric vector of p-values.
#' @param type Output type. Can be "latex", "html" or "markdown"
#' @param sign_levels Vector of significance levels from less to more strict
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#'
#' @import dplyr
add_stars <- function(
  p.value,
  type = "html",
  sign_levels = c("*" = .1, "**" = .05, "***" = .01)) {

  out <- rep("", length(p.value))

  if (type == "latex") {
    for (lvl in seq_along(sign_levels)) {
      out <- dplyr::if_else(
        p.value <= sign_levels[lvl],
        true = paste0("^{", names(sign_levels)[lvl], "}"),
        false = out)
    }
  } else {
    for (lvl in seq_along(sign_levels)) {
      out <- dplyr::if_else(
        p.value <= sign_levels[lvl],
        true = names(sign_levels)[lvl],
        false = out)
    }
  }

  return(out)

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



#' Fix names of variables after join operation
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .data DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
fix_join <- function(.data) {
  names(.data) <- gsub(pattern = ".x$", replacement = "", x = names(.data))
  names(.data) <- gsub(pattern = ".y$", replacement = "_merged", x = names(.data))
  return(.data)
}
