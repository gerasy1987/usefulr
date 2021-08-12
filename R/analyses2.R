#' Estimation Function
#'
#' @param dv Dependent variable specified as character.
#' @param treat Treatment vector of variables specified as character vector.
#' @param covs Character vector of covariates specified as character vector.
#' @param heterogenous Character vector of covariates to interact with treatments specified as character vector.
#' @param subset character string specifing logical expression for subsetting.
#' @param FE Character vector of fixed effects covariates specified as character vector.
#' @param IPW Inverse probability weights specified as character vector.
#' @param IV_list Character string. Should be a character string which presents valid IV formula as specified in \code{lfe::felm}.
#' @param cluster Covariate for clustered robust standard errors as defined by \code{multiwayvcov::cluster.vcov} function. Currently only one clustering variable option supported specified as character vector.
#' @param robust Logical. Whether to report heteroskedastic robust standard errors. Implemented only for linear models for now.
#' @param data Data frame which contains all the relevant variables.
#' @param estimator Function that takes all arguments above, \code{fit_formula} and \code{frame_df} as arguments and returns a tibble with the following columns: "term" (required), "estimate" (required), "std.error" (optional), "p.value" (required).
#' @param estimator_name Character string giving name of the estimator used
#' @param treat_only Logical vector of length 1, specifying whether only \code{treat} estimates should be reported. Defaults to \code{FALSE}.
#' @param status Logical vector of length 3, specifying whether the model was pre-(R)egistered, run in (S)cript and reported in (P)aper respectively.
#' @param round_digits Integer. How many decimal points to round to in the output.
#' @param return_df If \code{TRUE} dataframe used for estimation will be returned.
#' @param ... Other parameters used in estimate function
#' @return List of three objects. \code{estimates} is estimates from the model and corresponding standard errors. \code{stat} is vector of adjusted R squared and number of observations. \code{model_spec} is logical vector of characteristics of the model.
#' @examples
#'
#' @import dplyr
#' @import broom
#' @importFrom purrr when
#' @export


make_analyses <- function(dv,
                      treat,
                      covs = NULL,
                      heterogenous = NULL,
                      FE = NULL,
                      IPW = NULL,
                      IV_list = NULL,
                      cluster = NULL,
                      robust = is.null(cluster),
                      data,
                      subset = NULL,
                      estimator,
                      estimator_name,
                      treat_only = FALSE,
                      status = NULL,
                      # stars = FALSE,
                      round_digits = 3,
                      return_df = FALSE,
                      ...) {

  frame_formula <-
    stats::as.formula(paste(dv, "~", paste(unique(c(treat, covs, FE, cluster, IPW,
                                                    heterogenous, IV_list$dv, IV_list$instr)),
                                           collapse = " + ")))
  if (is.null(heterogenous)) {
    main_formula <- paste(setdiff(c("1", treat, covs), IV_list$dv), collapse = " + ")
  } else {
    main_formula <-
      paste(
        setdiff(c("1", treat, paste0(treat, ":", heterogenous), heterogenous, covs),
                IV_list$dv),
        collapse = " + ")
  }
  main_formula <- paste(dv, "~", main_formula)
  # main_formula_FE <-
  #   ifelse(!is.null(FE),
  #          paste0(main_formula, paste0(paste0(" + factor(", FE, ")"), collapse = "")),
  #          main_formula)

  FE_formula <- dplyr::if_else(is.null(FE), "0", paste(FE, collapse = "+"))

  if (!is.null(IV_list)) {
    IV_formula <- paste("(",
                        paste0(IV_list$dv, collapse = "|"), "~",
                        paste0(ifelse(test = IV_list$factor,
                                      yes = paste0("factor(", IV_list$instr, ")"),
                                      no = IV_list$instr), collapse = "+"),
                        ")")
  } else {
    IV_formula <- "0"
  }

  cluster_formula <- dplyr::if_else(is.null(cluster), "0", paste(cluster, collapse = "+"))

  fit_formula <-
    stats::as.formula(paste(main_formula, "|",
                            FE_formula, "|",
                            IV_formula, "|",
                            cluster_formula))

  frame_df <-
    purrr::when(subset,
                is.character(.) ~ dplyr::filter(data, eval(parse(text =  subset))),
                ~ data)
  frame_df <-
    dplyr::filter(frame_df,
                  dplyr::if_all(unique(c(treat, dv, FE,
                                         cluster, IPW, heterogenous,
                                         IV_list$dv, IV_list$instr)),
                                ~ !is.na(.)))

  frame_df <- stats::model.frame(frame_formula, data = frame_df)

  frame_df <-
    dplyr::mutate(
      frame_df,
      across(all_of(FE), ~ as.factor(.))
    )

  # if (length(FE) > 1) frame_df[, FE] <- (plyr::colwise(as.factor))(frame_df[, FE])
  # if (length(FE) == 1) frame_df[, FE] <- as.factor(frame_df[, FE])

  # ESTIMATION

  col_names <- c("term", "estimate", "std.error", "p.value")

  estout <-
    estimator(dv = dv,
              treat = treat,
              covs = covs,
              heterogenous = heterogenous,
              FE = FE,
              IPW = IPW,
              IV_list = IV_list,
              cluster = cluster,
              robust = robust,
              data = data,
              subset = subset,
              fit_formula = fit_formula,
              main_formula = main_formula,
              FE_formula = FE_formula,
              IV_formula = IV_formula,
              cluster_formula = cluster_formula,
              frame_df = frame_df,
              col_names = col_names,
              ...)

  # check consistency of estout format
  missing_col_names <- setdiff(col_names, names(estout$estimates))

  if (length(missing_col_names) != 0) {
    if (missing_col_names != "std.error")
      stop("estimate function has to return tibble with 'term', 'estimate' and 'p.value' columns.")

    if (missing_col_names == "std.error") estout$estimates$std.error <- NA
  }

  # cleanup estout$estimates
  estout$estimates <- estout$estimates[!grepl(pattern = "Intercept", x = estout$estimates$term, fixed = TRUE),]
  if (treat_only) estout$estimates <- estout$estimates[grepl(pattern = paste(paste0(treat), collapse = "|"), x = estout$estimates$term),]

  # # add stars and printout column
  # if (stars) {
  #   out <-
  #     dplyr::mutate(estout$estimates,
  #                   printout =
  #                     ifelse(is.nan(estimate), "-- [--]",
  #                            ifelse(is.na(std.error),
  #                                   paste0(fround(estimate, digits = round_digits),
  #                                          add_stars(p.value = p.value),
  #                                          " [", fround(p.value, digits = round_digits), "]"),
  #                                   paste0(fround(estimate, digits = round_digits),
  #                                          add_stars(p.value = p.value),
  #                                          " [", fround(std.error, digits = round_digits), "]"))),
  #                   estimate = round(estimate, digits = round_digits),
  #                   std.error = round(std.error, digits = round_digits),
  #                   p.value = round(p.value, digits = round_digits))
  # } else if (!stars) {
  #   out <-
  #     dplyr::mutate(estout$estimates,
  #                   printout =
  #                     ifelse(is.nan(estimate), "-- [--]",
  #                            ifelse(is.na(std.error),
  #                                   paste0(fround(estimate, digits = round_digits),
  #                                          " [", fround(p.value, digits = round_digits), "]"),
  #                                   paste0(fround(estimate, digits = round_digits),
  #                                          " [", fround(std.error, digits = round_digits), "]"))),
  #                   estimate = round(estimate, digits = round_digits),
  #                   std.error = round(std.error, digits = round_digits),
  #                   p.value = round(p.value, digits = round_digits))
  # }



  out <- dplyr::select(.data = estout$estimates,
                       term, estimate, std.error, p.value)

  list_out <-
    list(estimates = out,
         stat = c(
           r.squared = ifelse(!is.null(estout$r2), fround(estout$r2, digits = round_digits), NA),
           #   ifelse(test = (model == "lm"),
           #          yes = fround(summary(fit)$r2adj, digits = 3),
           #          no = fround(r2_log_prob, digits = 3)),
           n_obs = fround(nrow(frame_df), digits = 0)),
         model_spec = c(
           MODEL = estimator_name,
           HETEROGENOUS =
             dplyr::if_else(!is.null(heterogenous), paste(heterogenous, collapse = ", "), NA_character_),
           FE =
             dplyr::if_else(!is.null(FE), paste(FE, collapse = ", "), "no"),
           commaCLUSTER =
             dplyr::if_else(!is.null(cluster), paste(cluster, collapse = ", "), "no"),
           IPW =
             dplyr::if_else(!is.null(IPW), paste(IPW, collapse = ", "), "no")),
         model_status = c(
           R = ifelse(status[1] == 1, "yes", "no"),
           S = ifelse(status[2] == 1, "yes", "no"),
           P = ifelse(status[3] == 1, "yes", "no")),
         internals = list(
           data = purrr::when(return_df, . ~ frame_df, ~ NULL),
           fit = estout$fit,
           estfun_formula =
             paste(main_formula, "|", FE_formula, "|", IV_formula, "|", cluster_formula),
           main_formula = main_formula,
           FE_formula = FE_formula,
           IV_formula = IV_formula,
           cluster_formula = cluster_formula
         )
    )


  return(structure(list_out,
                   class = c("analyses_list", "list")) )

}
