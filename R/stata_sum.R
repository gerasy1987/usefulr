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
#'
#' @import dplyr
#' @importFrom tidyr gather

stata_sum <- function(.vars,
                      .data,
                      .percentile = c(.25, .75),
                      .by = NULL) {

  if (length(unique(.vars)) != length(.vars)) stop("Non-unique variable names are provided.")

  if (is.null(.by)) {

    .data %>%
      dplyr::select(all_of(.vars)) %>%
      tidyr::gather(key = "variable", value = "value") %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(
        across(c(value),
               list(non_NAs = ~ sum(!is.na(.x)),
                    mean = ~ mean(.x, na.rm = TRUE),
                    sd = ~ sd(.x, na.rm = TRUE),
                    min = ~ min(.x, na.rm = TRUE),
                    p1 = ~ quantile(.x, probs = min(.percentile), na.rm = TRUE, type = 2),
                    p50 = ~ median(.x, na.rm = TRUE),
                    p99 = ~ quantile(.x, probs = max(.percentile), na.rm = TRUE, type = 2),
                    max = ~ max(.x, na.rm = TRUE),
                    NAs = ~ sum(is.na(.x)),
                    unique = ~ length(unique(na.omit(.x)))),
               .names = "{.fn}")) %>%
      `names<-`(c("variable", "non_NAs", "mean", "sd",
                  "min", paste0("p", 100*min(.percentile)),
                  "p50", paste0("p", 100*max(.percentile)),
                  "max", "NAs", "unique")) %>%
      {.[order(base::match(.$variable, .vars)),]}

  } else if (!is.null(.by)) {

    .data %>%
      dplyr::select(all_of(c(.vars, .by))) %>%
      tidyr::gather(key = "variable", value = "value", -.by) %>%
      dplyr::group_by(across(c(all_of(.by), variable))) %>%
      dplyr::summarise(
        across(c(value),
               list(non_NAs = ~ sum(!is.na(.x)),
                    mean = ~ mean(.x, na.rm = TRUE),
                    sd = ~ sd(.x, na.rm = TRUE),
                    min = ~ min(.x, na.rm = TRUE),
                    p1 = ~ quantile(.x, probs = min(.percentile), na.rm = TRUE, type = 2),
                    p50 = ~ median(.x, na.rm = TRUE),
                    p99 = ~ quantile(.x, probs = max(.percentile), na.rm = TRUE, type = 2),
                    max = ~ max(.x, na.rm = TRUE),
                    NAs = ~ sum(is.na(.x)),
                    unique = ~ length(unique(na.omit(.x)))),
               .names = "{.fn}")) %>%
      `names<-`(c(.by, "variable", "non_NAs", "mean", "sd",
                  "min", paste0("p", 100*min(.percentile)),
                  "p50", paste0("p", 100*max(.percentile)),
                  "max", "NAs", "unique")) %>%
      {.[order(base::match(.$variable, .vars)),]}

  }
}
