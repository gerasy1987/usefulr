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


#' LaTeX Helper to Create a Cell Linebreak by Symbol Length (Excluding Math)
#'
#' @param x DESCRIPTION.
#' @param align DESCRIPTION.
#' @param double_escape DESCRIPTION.
#' @param symbol_width DESCRIPTION.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr stringr
linebreak2 <- function(
  x, align = c("l", "c", "r"), double_escape = F, symbol_width = 11
) {
  if (is.numeric(x) | is.logical(x)) return(x)

  x_start <- stringr::str_length(str_extract(pattern = "\\$.*\\$ ", string = x))

  word_breaks <-
    lapply(
      stringr::str_locate_all(pattern = ' ', gsub("\\$.*\\$ ", "", as.character(x))),
      function(stri) {
        stri <- unname(stri[,1])
        stri[which((stri %/% symbol_width) != lag(stri %/% symbol_width))]
      })

  for (i in seq_along(x)) {
    if (length(word_breaks[[i]]) > 0) {
      strintr::str_sub(x[i],
                       start = x_start[i] + word_breaks[[i]],
                       end = x_start[i] + word_breaks[[i]]) <- ";;;"
    }
  }

  align <- vapply(align, match.arg, "a", choices = c("l", "c", "r"))
  if (double_escape) {
    ifelse(strintr::str_detect(x, ";;;"),
           paste0("\\\\makecell[", align, "]{", strintr::str_replace_all(x, ";;;", "\\\\\\\\\\\\\\\\"), "}"),
           x)
  } else {
    ifelse(strintr::str_detect(x, ";;;"),
           paste0("\\makecell[", align, "]{", strintr::str_replace_all(x, ";;;", "\\\\\\\\"), "}"),
           x)
  }
}
