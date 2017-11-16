#' Analysis Function for Replications.
#'
#' @param DV Dependent variable specified as character.
#' @param treat Treatment vector of variables specified as character vector.
#' @param data Data frame which contains all the relevant variables.
#' @param model Character string specifying the model to estimate. Currently only "lm" (OLS), "logit" (Binary Logit), "probit" (Binary Probit), "ologit" (Ordered Logit) and "oprobit" (Ordered Probit) models are supported. Default is "lm".
#' @param covs Character vector of covariates specified as character vector.
#' @param heterogenous Character vector of covariates to interact with treatments specified as character vector.
#' @param subset character string specifing logical expression for subsetting.
#' @param FE Character vector of fixed effects covariates specified as character vector.
#' @param cluster Covariate for clustered robust standard errors as defined by \code{multiwayvcov::cluster.vcov} function. Currently only one clustering variable option supported specified as character vector.
#' @param IV_list Character string. Should be a character string which presents valid IV formula as specified in \code{lfe::felm}.
#' @param robust Logical. Whether to report heteroskedastic robust standard errors. Implemented only for linear models for now.
#' @param IPW Inverse probability weights specified as character vector.
#' @param margin_at Character string which should be in the format of \code{'var_name = value'}, defaults to NULL (no marginal effects). This calculates the marginal effects of the \code{treat} variable in Logit and Probit models at these particular levels. Takes only binary variables.
#' @param treat_only Logical vector of length 1, specifying whether only \code{treat} estimates should be reported. Defaults to \code{FALSE}.
#' @param status Logical vector of length 3, specifying whether the model was pre-(R)egistered, run in (S)cript and reported in (P)aper respectively.
#' @param stars Logical. If \code{FALSE} no stars are passed to printout.
#' @param round_digits Integer. How many decimal points to round to in the output.
#' @param return_df If \code{TRUE} dataframe used for estimation will be returned.
#' @return List of three objects. \code{estimates} is estimates from the model and corresponding standard errors. \code{stat} is vector of adjusted R squared and number of observations. \code{model_spec} is logical vector of characteristics of the model.
#' @examples
#' \dontrun{
#' usefulr::analyses(DV = "mpg",
#'                   treat = "vs",
#'                   model = "lm",
#'                   covs = c("carb", "disp"),
#'                   heterogenous = "cyl",
#'                   IPW = "drat",
#'                   cluster = "gear",
#'                   subset = "qsec >= 16",
#'                   FE = "vs",
#'                   treat_only = FALSE,
#'                   status = c(T,T,F),
#'                   data = mtcars,
#'                   margin_at = NULL)
#'          }
#'
#' @import dplyr broom
#' @importFrom lfe felm
#' @importFrom multiwayvcov cluster.vcov
#' @importFrom lmtest coeftest
#' @importFrom MASS polr
#' @importFrom margins margins
#' @export

analyses <- function(DV,
                     treat,
                     data,
                     model = "lm",
                     covs = NULL,
                     heterogenous = NULL,
                     subset = NULL,
                     FE = NULL,
                     cluster = NULL,
                     IV_list = NULL,
                     robust = !is.null(cluster),
                     IPW = NULL,
                     treat_only = FALSE,
                     margin_at = NULL,
                     status = NULL,
                     stars = FALSE,
                     round_digits = 3,
                     return_df = FALSE) {

  # required packages
  requireNamespace("plyr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("broom", quietly = TRUE)
  requireNamespace("lmtest", quietly = TRUE)

  if (model != "lm" & is.null(cluster) & robust)
    warning("Heteroskedastic robust SE are only implemented for model = 'lm' at this moment. Regular SE will be reported")

  if (model != "lm" & !is.null(IV_list))
    warning("Instrumental variables are only implemented for model = 'lm' at this time. No instrumental variable estimates are reported")

  frame_formula <-
    stats::as.formula(paste(DV, "~", paste(c(treat, covs, FE, cluster, IPW,
                                             heterogenous, IV_list$dv, IV_list$instr),
                                           collapse = " + ")))
  if (is.null(heterogenous)) {
    main_formula <- paste(c(treat, covs), collapse = " + ")
  } else {
    main_formula <- paste(c(treat, paste0(treat, ":", heterogenous),
                            heterogenous, covs), collapse = " + ")
  }
  main_formula    <- paste(DV, "~", main_formula)
  main_formula_FE <-
    ifelse(!is.null(FE),
           paste0(main_formula, paste0(paste0(" + factor(", FE, ")"), collapse = "")),
           main_formula)
  FE_formula      <- ifelse(is.null(FE), 0, paste(FE, collapse = "+"))
  if (!is.null(IV_list)) {
    IV_formula <- paste("(",
                        paste0(IV_list$dv, collapse = "|"), "~",
                        paste0(ifelse(test = IV_list$factor,
                                      yes = paste0("factor(", IV_list$instr, ")"),
                                      no = IV_list$instr), collapse = "+"),
                        ")")
  } else {
    IV_formula <- 0
  }
  cluster_formula <- ifelse(is.null(cluster), 0, paste(cluster,
                                                       collapse = "+"))
  fit_formula <- stats::as.formula(paste(main_formula, "|",
                                         FE_formula, "|", IV_formula, "|", cluster_formula))

  frame_df <- eval(parse(text = paste0("dplyr::filter(.data = data, ", subset, ")")))
  frame_df <- eval(parse(text = paste0("dplyr::filter(.data = frame_df, ",
                                       paste(
                                         paste0("!is.na(",
                                                c(treat, DV, FE, cluster, IPW, heterogenous,
                                                  IV_list$dv, IV_list$instr), ")"),
                                         collapse = " & "), ")" )))
  frame_df <- stats::model.frame(frame_formula, data = frame_df)

  if (length(FE) > 1) frame_df[, FE] <- (plyr::colwise(as.factor))(frame_df[, FE])
  if (length(FE) == 1) frame_df[, FE] <- as.factor(frame_df[, FE])

  ## ESTIMATION

  if (model == "lm") {

    requireNamespace("lfe", quietly = TRUE)

    if (is.null(IPW)) {
      fit <-
        suppressWarnings(
          summary(lfe::felm(formula = fit_formula,
                            data = frame_df), robust = robust)
          )
    } else {
      fit <-
        suppressWarnings(
          summary(lfe::felm(formula = fit_formula,
                    data = frame_df,
                    weights = unlist(frame_df[, IPW])), robust = robust)
        )
    }

  } else if (!is.null(heterogenous)) {

    stop("Logit and Probit models with hetorogenous effects are not supported (YET!).\n")

  } else if (is.null(heterogenous)) {

    if (model %in% c("logit", "probit")) {

      ## LOGISTIC AND PROBIT MODELS

      requireNamespace("multiwayvcov", quietly = TRUE)
      requireNamespace("margins", quietly = TRUE)
      requireNamespace("pscl", quietly = TRUE)

      fit <-
        suppressWarnings(
          stats::glm(formula = stats::as.formula(main_formula_FE),
                     data = frame_df,
                     family = stats::binomial(link = model),
                     weights =
                       if (!is.null(IPW)) unlist(frame_df[, IPW]))
        )

      llh <-  logLik(fit)
      llhNull <- logLik(update(fit, formula. = ~1))

      r2_log_prob <- 1 - ((llh - attr(llh, which = "df") + attr(llhNull, which = "df")) / llhNull)

      if (is.null(margin_at)) {

        fit <-
          suppressWarnings(
            lmtest::coeftest(fit,
                             vcov =
                               if (!is.null(cluster))
                                 suppressWarnings(
                                   multiwayvcov::cluster.vcov(model = fit,
                                                              cluster = frame_df[, cluster]))
            )
          )

        fit <- dplyr::filter(.data = broom::tidy(fit),
                             !base::grepl(pattern = 'factor', x = term))

      } else if (!is.null(margin_at)) {

        if (!all(frame_df[, treat] %in% 0:1))
          stop("Marginal effects for non-binary variables are not supported.\n")


        fit <-
          suppressWarnings(
            summary(
              margins::margins(model = fit,
                               variables = treat,
                               type = "response",
                               vce = "delta",
                               vcov =
                                 if (!is.null(cluster))
                                   suppressWarnings(
                                     multiwayvcov::cluster.vcov(model = fit, cluster = frame_df[, cluster]))
              )
            )
          )

        fit <- fit[c("factor", "AME", "SE", "p")]
        colnames(fit) <- c("term", "estimate", "std.error", "p.value")

      }

    } else if (model %in% c("ologit", "oprobit")) {

      ## ORDERED LOGISTIC AND PROBIT MODELS

      requireNamespace("MASS", quietly = TRUE)
      requireNamespace("pscl", quietly = TRUE)

      frame_df[,DV] <- as.factor(frame_df[,DV])

      fit <-
        suppressWarnings(
          MASS::polr(formula = stats::as.formula(main_formula_FE),
                     data = frame_df,
                     method = ifelse(model == "ologit", "logistic", "probit"),
                     weights =
                       if (!is.null(IPW)) unlist(frame_df[, IPW]),
                     Hess = TRUE)
        )


      llh <-  logLik(fit)
      llhNull <- logLik(update(fit, formula. = ~1))

      r2_log_prob <- as.numeric(1 - ((llh - attr(llh, which = "df") + attr(llhNull, which = "df")) / llhNull))

      if (is.null(margin_at)) {

        fit <-
          suppressWarnings(
            lmtest::coeftest(fit,
                             vcov =
                               if (!is.null(cluster)) vcov_cluster(fit,
                                                                   .cluster = frame_df[, cluster])
            )
          )

        fit <- dplyr::filter(.data = broom::tidy(fit),
                             !base::grepl(pattern = 'factor', x = term))

      } else if (!is.null(margin_at)) {

        requireNamespace("margins", quietly = TRUE)

        fit <-
          suppressWarnings(
            ocme_mod(w = fit,
                     vcov = if (!is.null(cluster)) vcov_cluster(fit,
                                                                .cluster = frame_df[, cluster])
            )$out
          )


        if ( !(paste0("ME.", margin_at) %in% names(fit)) )
          stop("margin_at is out of bounds of dependent variable.")

        fit <- fit[[paste0("ME.", margin_at)]]


        fit <- dplyr::filter(.data = fit,
                             !base::grepl(pattern = 'factor', x = term))

      }

    }
  }
  col_names <- c("term", "estimate", "std.error", "p.value")

  if (model %in% c("logit", "probit", "ologit", "oprobit")) {
    estout <- fit[, col_names]
  } else {
    # FE as-if in Stata
    # if (!is.null(FE)) {
    #     icpt <- unname(plyr::name_rows( lfe::getfe(fit, ef = function(gamma,
    #         addnames) absorb(gamma = gamma, addnames = addnames,
    #         FE = frame_df[, FE]), se = T, bN = 1000, cluster = TRUE)))
    #     icpt <- cbind(icpt[c(5, 1, 4)], pval = 2 * stats::pt(unlist(icpt[1])/unlist(icpt[4]),
    #         df = suppressWarnings(broom::glance(fit)[, "df"]),
    #         lower.tail = FALSE))
    #     colnames(icpt) <- col_names
    #     estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[,
    #         col_names]))
    # }
    # else {
    estout <- broom::tidy(fit$coefficients)[,c(1:3,5)]
    colnames(estout) <- col_names
  }

  estout <- estout[!grepl(pattern = "(Intercept)", x = estout$term, fixed = TRUE),]
  if (treat_only) estout <- estout[grepl(pattern = paste(paste0("^", treat, "$"), collapse = "|"), x = estout$term),]


  out <-
    dplyr::mutate(estout,
                  printout =
                    ifelse(is.nan(estimate), "-- [--]",
                           paste0(fround(estimate, digits = round_digits),
                                  ifelse(stars, add_stars(p.value), ""),
                                  " [", fround(std.error, digits = round_digits), "]")),
                  estimate = round(estimate, digits = round_digits),
                  std.error = round(std.error, digits = round_digits),
                  p.value = round(p.value, digits = round_digits))

  out <- dplyr::select(.data = out,
                       term, estimate, std.error,
                       printout, p.value)

  list_out <-
    list(estimates = out,
         stat = c(r.squared =
                    ifelse(test = (model == "lm"),
                           yes = fround(fit$r2adj, digits = 3),
                           no = fround(r2_log_prob, digits = 3)),
                  n_obs = fround(nrow(frame_df), digits = 0)),
         model_spec = c(MODEL = model,
                        HETEROGENOUS =
                          ifelse(!is.null(heterogenous),
                                 paste(heterogenous, collapse = ", "), NA),
                        FE = ifelse(!is.null(FE),
                                    paste(FE, collapse = ", "), "no"),
                        commaCLUSTER = ifelse(!is.null(cluster),
                                              paste(cluster, collapse = ", "), "no"),
                        IPW = ifelse(!is.null(IPW),
                                     paste(IPW, collapse = ", "), "no")),
         model_status = c(R = ifelse(status[1] ==
                                       1, "yes", "no"), S = ifelse(status[2] == 1, "yes", "no"),
                          P = ifelse(status[3] == 1, "yes", "no")),
         internals = list(data = if (return_df) frame_df else NULL,
                          estfun_formula =
                            ifelse((model == "lm"),
                                   paste(main_formula, "|",
                                         FE_formula, "|", IV_formula, "|", cluster_formula),
                                   main_formula_FE) ))


  return(structure(list_out,
                   class = c("analyses_list", "list")) )

}


#' @export
print.analyses_list <- function(analyses_list) {
  cat("\nEstimation formula:\n")
  print(analyses_list$internals$estfun_formula)
  cat("\nEstimates:\n")
  print(analyses_list$estimates[,c("term","estimate","std.error")])
  cat("\nSummary:\nAdj. R2 =", analyses_list$stat[1], ", N =", analyses_list$stat[2],"\n")
  invisible(analyses_list)
}
