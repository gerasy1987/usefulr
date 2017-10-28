#' Analysis function for EGAP replication.
#'
#' @param .DV Dependent variable specified as character.
#' @param .treat .treatment Character vector of variables specified as character vector.
#' @param .covs Character vector of covariates specified as character vector.
#' @param .heterogenous Character vector of covariates to interact with treatments specified as character vector.
#' @param .subset character string specifing logical expression for subsetting.
#' @param .FE Character vector of fixed effects covariates specified as character vector.
#' @param .cluster Covariate for clustered robust standard errors as defined by \code{multiwayvcov::cluster.vcov} function. Currently only one clustering variable option supported specified as character vector.
#' @param .IPW Inverse probability weights specified as character vector.
#' @param .data Data frame which contains all the relevant variables.
#' @param .model Character string specifying the model to estimate. Currently only "lm" and "logit"/"probit" and "ologit"/"oprobit" is supported. Default is "lm".
#' @param .margin_at Character string which should be in the format of \code{'var_name = value'}, defaults to NULL (no marginal effects). This calculates the marginal effects of the \code{.treat} variable in Logit and Probit models at these particular levels. Takes only binary variables.
#' @param .treat_only Logical vector of length 1, specifying whether only \code{.treat} estimates should be reported. Defaults to \code{FALSE}.
#' @param .status Logical vector of length 3, specifying whether the model was pre-(R)egistered, run in (S)cript and reported in (P)aper respectively.
#' @return List of three objects. \code{estimates} is estimates from the model and corresponding standard errors. \code{stat} is vector of adjusted R squared and number of observations. \code{model_spec} is logical vector of characteristics of the model.
#' @examples
#' \dontrun{
#' usefulr::analyses(.DV = "mpg",
#'                   .treat = "vs",
#'                   .model = "lm",
#'                   .covs = c("carb", "disp"),
#'                   .heterogenous = "cyl",
#'                   .IPW = "drat",
#'                   .cluster = "gear",
#'                   .subset = "qsec >= 16",
#'                   .FE = "vs",
#'                   .treat_only = FALSE,
#'                   .status = c(T,T,F),
#'                   .data = mtcars,
#'                   .margin_at = NULL)
#'          }
#'
#' @import dplyr broom
#' @importFrom lfe felm
#' @importFrom multiwayvcov cluster.vcov
#' @importFrom lmtest coeftest
#' @importFrom modmarg marg
#' @importFrom MASS polr
#' @importFrom margins margins
#' @export

analyses <- function(.DV,
                     .treat,
                     .covs = NULL,
                     .heterogenous = NULL,
                     .subset = NULL,
                     .FE = NULL,
                     .cluster = NULL,
                     .IPW = NULL,
                     .data,
                     .model = "lm",
                     .treat_only = FALSE,
                     .margin_at = NULL,
                     .status = NULL,
                     .stars = FALSE) {

  # required packages
  requireNamespace("plyr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("broom", quietly = TRUE)
  # requireNamespace("Hmisc", quietly = TRUE)
  requireNamespace("lmtest", quietly = TRUE)

  # if (!is.null(.FE) & .model != "lm")
  #   stop("Function does not support fixed effects for other than OLS models")
  #

  frame_formula <-
    stats::as.formula(paste(.DV, "~", paste(c(.treat, .covs, .FE, .cluster, .IPW, .heterogenous),
                                            collapse = " + ")))
  if (is.null(.heterogenous)) {
    main_formula <- paste(c(.treat, .covs), collapse = " + ")
  } else {
    main_formula <- paste(c(.treat, paste0(.treat, ":", .heterogenous),
                            .heterogenous, .covs), collapse = " + ")
  }
  main_formula    <- paste(.DV, "~", main_formula)
  main_formula_FE <-
    ifelse(!is.null(.FE),
           paste0(main_formula, paste0(paste0(" + factor(", .FE, ")"), collapse = "")),
           main_formula)
  FE_formula      <- ifelse(is.null(.FE), 0, paste(.FE, collapse = "+"))
  cluster_formula <- ifelse(is.null(.cluster), 0, paste(.cluster,
                                                        collapse = "+"))
  fit_formula <- stats::as.formula(paste(main_formula, "|",
                                         FE_formula, "|", 0, "|", cluster_formula))

  frame_df <- dplyr::filter_(.data = .data, .dots = .subset)
  frame_df <- dplyr::filter_(.data = frame_df,
                             .dots =
                               paste(
                                 paste0("!is.na(",
                                        c(.treat, .DV, .FE, .cluster, .IPW, .heterogenous), ")"),
                                 collapse = " & "))
  frame_df <- stats::model.frame(frame_formula, data = frame_df)

  if (length(.FE) > 1) frame_df[, .FE] <- (plyr::colwise(as.factor))(frame_df[, .FE])
  if (length(.FE) == 1) frame_df[, .FE] <- as.factor(frame_df[, .FE])

  ## ESTIMATION

  if (.model == "lm") {

    requireNamespace("lfe", quietly = TRUE)

    if (is.null(.IPW)) {
      fit <-
        suppressWarnings(
          lfe::felm(formula = fit_formula,
                    data = frame_df))
    } else {
      fit <-
        suppressWarnings(
          lfe::felm(formula = fit_formula,
                    data = frame_df,
                    weights = unlist(frame_df[, .IPW]))
        )
    }

  } else if (!is.null(.heterogenous)) {

    stop("Logit and Probit models with hetorogenous effects are not supported (YET!).\n")

  } else if (is.null(.heterogenous)) {

    if (.model %in% c("logit", "probit")) {

      ## LOGISTIC AND PROBIT MODELS

      requireNamespace("multiwayvcov", quietly = TRUE)
      requireNamespace("modmarg", quietly = TRUE)

      fit <-
        suppressWarnings(
          stats::glm(formula = stats::as.formula(main_formula_FE),
                     data = frame_df,
                     family = stats::binomial(link = .model),
                     weights =
                       if (!is.null(.IPW)) unlist(frame_df[, .IPW]))
        )


      if (is.null(.margin_at)) {

        fit <-
          suppressWarnings(
            lmtest::coeftest(fit,
                             vcov =
                               if (!is.null(.cluster))
                                 suppressWarnings(
                                   multiwayvcov::cluster.vcov(model = fit, cluster = frame_df[, .cluster]))
            )
          )

        fit <- dplyr::filter_(.data = broom::tidy(fit),
                              .dots = "!base::grepl(pattern = 'factor', x = term)")

      } else if (!is.null(.margin_at)) {

        if (!all(frame_df[, .treat] %in% 0:1))
          stop("Marginal effects for non-binary variables are not supported.\n")

        warning("Marginal effects for Logit and Probit models are only reported for .treat variable.\n")

        fit <-
          suppressWarnings(
            modmarg::marg(mod = fit,
                          var_interest = .treat,
                          type = "effects",
                          vcov_mat =
                            if (!is.null(.cluster))
                              suppressWarnings(
                                multiwayvcov::cluster.vcov(model = fit, cluster = frame_df[, .cluster]))
            )
          )

        fit <- fit[[1]][fit[[1]]$Label == paste(.treat,"=",.margin_at),
                        c("Label", "Margin", "Standard.Error", "P.Value")]
        colnames(fit) <- c("term", "estimate", "std.error", "p.value")

      }

    } else if (.model %in% c("ologit", "oprobit")) {

      ## ORDERED LOGISTIC AND PROBIT MODELS

      requireNamespace("MASS", quietly = TRUE)
      requireNamespace("margins", quietly = TRUE)

      frame_df[,.DV] <- factor(frame_df[,.DV])

      fit <-
        suppressWarnings(
          MASS::polr(formula = stats::as.formula(main_formula_FE),
                     data = frame_df,
                     method = ifelse(.model == "ologit", "logistic", "probit"),
                     weights =
                       if (!is.null(.IPW)) unlist(frame_df[, .IPW]),
                     Hess = TRUE)
        )

      if (is.null(.margin_at)) {

        fit <-
          suppressWarnings(
            lmtest::coeftest(fit,
                             vcov =
                               if (!is.null(.cluster)) vcov_cluster(fit,
                                                                    .cluster = frame_df[, .cluster])
            )
          )

        fit <- dplyr::filter(.data = broom::tidy(fit),
                             !base::grepl(pattern = 'factor', x = term))

      } else if (!is.null(.margin_at)) {

        fit <-
          suppressWarnings(
            ocme_mod(w = fit,
                     vcov = if (!is.null(.cluster)) vcov_cluster(fit,
                                                                 .cluster = frame_df[, .cluster])
            )$out[[paste0("ME.", .margin_at)]]
          )

        fit <- dplyr::filter(.data = fit,
                             !base::grepl(pattern = 'factor', x = term))

      }

    }
  }
  col_names <- c("term", "estimate", "std.error", "p.value")
  # if (!is.null(.FE)) {
  #     icpt <- unname(plyr::name_rows( lfe::getfe(fit, ef = function(gamma,
  #         addnames) absorb(gamma = gamma, addnames = addnames,
  #         .FE = frame_df[, .FE]), se = T, bN = 1000, cluster = TRUE)))
  #     icpt <- cbind(icpt[c(5, 1, 4)], pval = 2 * stats::pt(unlist(icpt[1])/unlist(icpt[4]),
  #         df = suppressWarnings(broom::glance(fit)[, "df"]),
  #         lower.tail = FALSE))
  #     colnames(icpt) <- col_names
  #     estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[,
  #         col_names]))
  # }
  # else {

  if (.model %in% c("logit", "probit", "ologit", "oprobit")) {
    estout <- fit[, col_names]
  } else {
    estout <- broom::tidy(fit)[, col_names]
  }

  if (.treat_only) estout <- estout[grepl(pattern = .treat, x = estout$term),]

  # estout[1, 1] <- "intercept"
  # }

  out <-
    dplyr::mutate(estout,
                  printout =
                    ifelse(is.nan(estimate), "-- [--]",
                           paste0(fround(estimate, digits = 3),
                                  ifelse(.stars, add_stars(p.value), ""),
                                  " [", fround(std.error, digits = 3), "]")),
                  estimate =
                    round(estimate, digits = 3), std.error = round(std.error,
                                                                   digits = 3),
                  p.value = round(p.value, digits = 3))

  out <- dplyr::select(.data = out, term, estimate, std.error,
                       printout, p.value)

  list(estimates = out,
       stat = c(adj.r.squared =
                  ifelse(.model == "lm", fround(broom::glance(fit)$adj.r.squared, digits = 3), NA),
                n_obs = fround(nrow(frame_df), digits = 0)),
       model_spec = c(MODEL = .model,
                      HETEROGENOUS =
                        ifelse(!is.null(.heterogenous),
                               paste(.heterogenous, collapse = " "), NA),
                      FE = ifelse(!is.null(.FE),
                                  paste(.FE, collapse = " "), "no"),
                      commaCLUSTER = ifelse(!is.null(.cluster),
                                            paste(.cluster, collapse = " "), "no"),
                      IPW = ifelse(!is.null(.IPW),
                                   paste(.IPW, collapse = " "), "no")),
       model_status = c(R = ifelse(.status[1] ==
                                     1, "yes", "no"), S = ifelse(.status[2] == 1, "yes", "no"),
                        P = ifelse(.status[3] == 1, "yes", "no")))

}
