#' Function to produce Stata-like variable(s) summary
#'
#' Produce variable(s) summary statistics
#'
#' @param .vars Character vector giving variables to be summarized
#' @param .data Data frame to be used for summary statistics construction
#' @param .percentile Numeric vector of length 2 giving two percentile levels to be produced. Both values in the vector should be between 0 and 1
#' @param .by Charatcer string giving variable to be used to break each variable summary by
#'
#' @return tibble object.
#' @export

stata_sum <- function(.vars,
                      .data,
                      .percentile = c(.25, .75),
                      .by = NULL) {

  requireNamespace("dplyr")
  requireNamespace("tidyr")

  if (is.null(.by)) {

    .data %>%
      dplyr::select_at(vars(.vars)) %>%
      tidyr::gather(key = "variable", value = "value") %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise_at(vars(value),
                          funs(non_NAs = sum(!is.na(.)),
                               mean = mean(., na.rm = TRUE),
                               sd = sd(., na.rm = TRUE),
                               min = min(., na.rm = TRUE),
                               !!paste0("p",100*min(.percentile)) := quantile(., probs = min(.percentile), na.rm = TRUE, type = 2),
                               p50 = median(., na.rm = TRUE),
                               !!paste0("p",100*max(.percentile)) := quantile(., probs = max(.percentile), na.rm = TRUE, type = 2),
                               max = max(., na.rm = TRUE),
                               NAs = sum(is.na(.)),
                               unique = length(unique(.))))

  } else if (!is.null(.by)) {

    .data %>%
      dplyr::select_at(vars(.vars, .by)) %>%
      tidyr::gather(key = "variable", value = "value", -.by) %>%
      dplyr::group_by_at(vars(.by, variable)) %>%
      dplyr::summarise_at(vars(value),
                          funs(non_NAs = sum(!is.na(.)),
                               mean = mean(., na.rm = TRUE),
                               sd = sd(., na.rm = TRUE),
                               min = min(., na.rm = TRUE),
                               !!paste0("p",100*min(.percentile)) := quantile(., probs = min(.percentile), na.rm = TRUE, type = 2),
                               p50 = median(., na.rm = TRUE),
                               !!paste0("p",100*max(.percentile)) := quantile(., probs = max(.percentile), na.rm = TRUE, type = 2),
                               max = max(., na.rm = TRUE),
                               NAs = sum(is.na(.)),
                               unique = length(unique(.))))

  }
}
