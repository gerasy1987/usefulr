#' Leave-m-out estimation
#'
#' @param data data frame
#' @param var character vector of variable(s) name(s) to use for LOO
#' @param loo_n number of unique values of var to leave out at a time
#' @param loo_fun estimation function
#' @param parallel whether to use parallel in \code{plyr::alply}
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @importFrom plyr alply
#' @importFrom RcppAlgos comboGeneral
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
loo_helper <-
  function(data,
           var,
           loo_n = 1,
           loo_fun =
             function(dat) estimatr::tidy(estimatr::lm_robust(data = dat, formula = mpg ~ 1)),
           parallel = FALSE
  ) {

    if (parallel) {
      requireNamespace(c("doParallel", "parallel"))
      doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    }

    .var <- base::apply(mtcars[var], 1, function(x) paste0(x, collapse = "_"))

    data %>%
      {
        plyr::alply(.data = RcppAlgos::comboGeneral(unique(.var), loo_n),
                    .margins = 1,
                    .fun = function(x) loo_fun(.[!(.var %in% x), ]),
                    .parallel = parallel)
      }
  }
