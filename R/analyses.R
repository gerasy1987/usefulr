#' Analysis function for EGAP replication.
#'
#' @param DV Dependent variable specified as character.
#' @param treat Treatment variable(s) specified as character vector.
#' @param covs Covariate(s) specified as character vector.
#' @param heterogenous Covariate(s) to interact with treatment(s) specified as character vector.
#' @param subset character string specifing logical expression for subsetting.
#' @param FE Fixed effects covariate(s) specified as character vector.
#' @param cluster Covariate for clustered robust standard errors as defined by multiwayvcov::cluster.vcov() function. Currently only one clustering variable option supported specified as character vector.
#' @param IPW Inverse probability weights specified as character vector.
#' @param data Data frame which contains all the relevant variables.
#' @param model Character string specifying the model to estimate. Currently only "lm" and "logit" is supported. Default is "lm".
#' @param status Logical vector of length 3, specifying whether the model was pre-(R)egistered, run in (S)cript and reported in (P)aper respectively.
#' @return List of three objects. \code{estimates} is estimates from the model and corresponding standard errors. \code{stat} is vector of adjusted R squared and number of observations. \code{model_spec} is logical vector of characteristics of the model.
#' @examples
#' \dontrun{
#' analyses(DV = "incumbent_share",
#'          treat = c('civics','infoonly'),
#'          covs = NULL,
#'          subset = "goodnews==1 & dosage==1",
#'          FE = "blid",
#'          heterogenous = NULL,
#'          IPW = "ipw",
#'          cluster = "newid",
#'          data = benin_admin,
#'          model = "lm",
#'          status = c(TRUE,TRUE,TRUE))
#'          }
#'
#' @import dplyr broom
#' @importFrom lfe felm
#' @importFrom multiwayvcov cluster.vcov
#' @importFrom lmtest coeftest
#' @importFrom stats binomial
#' @export

analyses <- function(DV,
                     treat,
                     covs = NULL,
                     heterogenous = NULL,
                     subset = NULL,
                     FE = NULL,
                     cluster = NULL,
                     IPW = NULL,
                     data,
                     model = "lm",
                     status = c(TRUE, TRUE, TRUE)) {

  # required packages
  requireNamespace("plyr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("broom", quietly = TRUE)
  requireNamespace("Hmisc", quietly = TRUE)
  requireNamespace("lfe", quietly = TRUE)
  requireNamespace("multiwayvcov", quietly = TRUE)
  requireNamespace("lmtest", quietly = TRUE)

  if (!is.null(FE) & model != "lm")
    stop("Function does not support FE for other than OLS models")
  frame_formula <- stats::as.formula(paste(DV, "~", paste(c(treat,
                                                            covs, FE, cluster, IPW, heterogenous), collapse = " + ")))
  if (is.null(heterogenous)) {
    main_formula <- paste(c(treat, covs), collapse = " + ")
  } else {
    main_formula <- paste(c(treat, paste0(treat, ":", heterogenous),
                            heterogenous, covs), collapse = " + ")
  }

  main_formula <- paste(DV, "~", main_formula)
  FE_formula <- ifelse(is.null(FE), 0, paste(FE, collapse = "+"))
  cluster_formula <- ifelse(is.null(cluster), 0, paste(cluster,
                                                       collapse = "+"))
  fit_formula <- stats::as.formula(paste(main_formula, "|",
                                         FE_formula, "|", 0, "|", cluster_formula))

  frame_df <- dplyr::filter_(.data = data, .dots = subset)
  frame_df <- dplyr::filter_(.data = frame_df,
                             .dots = paste(paste0("!is.na(",
                                                  c(treat, DV, FE, cluster, IPW, heterogenous), ")"),
                                           collapse = " & "))
  frame_df <- stats::model.frame(frame_formula, data = frame_df)

  if (length(FE) > 1)
    frame_df[, FE] <- (plyr::colwise(as.factor))(frame_df[,
                                                          FE])
  if (length(FE) == 1)
    frame_df[, FE] <- as.factor(frame_df[, FE])

  if (model == "lm") {

    if (is.null(IPW)) {
      fit <- lfe::felm(formula = fit_formula, data = frame_df)
    } else {
      fit <- lfe::felm(formula = fit_formula, data = frame_df,
                       weights = unlist(frame_df[, IPW]))
    }
  } else if (model == "logit") {

    if (is.null(IPW)) {
      fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula),
                                         data = frame_df, family = binomial(link = "logit")))
    } else {
      fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula),
                                         data = frame_df, weights = unlist(frame_df[,IPW]),
                                         family = binomial(link = "logit")))
    }

    if (!is.null(cluster)) {
      fit <- lmtest::coeftest(x = fit, vcov = multiwayvcov::cluster.vcov(model = fit,
                                                                         cluster = frame_df[, cluster]))
    }
  }
  col_names <- c("term", "estimate", "std.error", "p.value")
  # if (!is.null(FE)) {
  #     icpt <- unname(plyr::name_rows( lfe::getfe(fit, ef = function(gamma,
  #         addnames) absorb(gamma = gamma, addnames = addnames,
  #         .FE = frame_df[, FE]), se = T, bN = 1000, cluster = TRUE)))
  #     icpt <- cbind(icpt[c(5, 1, 4)], pval = 2 * stats::pt(unlist(icpt[1])/unlist(icpt[4]),
  #         df = suppressWarnings(broom::glance(fit)[, "df"]),
  #         lower.tail = FALSE))
  #     colnames(icpt) <- col_names
  #     estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[,
  #         col_names]))
  # }
  # else {
  estout <- broom::tidy(fit)[, col_names]
  # estout[1, 1] <- "intercept"
  # }

  out <-
    dplyr::mutate(estout,
                  printout =
                    ifelse(is.nan(estimate), "-- [--]",
                           paste0(fround(estimate, digits = 3),
                                  ifelse(p.value <= 0.01, "***",
                                         ifelse(p.value <= 0.05, "**",
                                                ifelse(p.value <= 0.1,"*",
                                                       ifelse(p.value <= 0.15, "+", "")))),
                                  " [", fround(std.error, digits = 3), "]")),
                  estimate =
                    round(estimate, digits = 3), std.error = round(std.error,
                                                                   digits = 3),
                  p.value = round(p.value, digits = 3))

  out <- dplyr::select(.data = out, term, estimate, std.error,
                       printout, p.value)

  list(estimates = out,
       stat = c(adj.r.squared =
                  ifelse(model ==
                           "lm", fround(broom::glance(fit)$adj.r.squared, digits = 3),
                         NA), n_obs = fround(nrow(frame_df), digits = 0)),
       model_spec = c(HETEROGENOUS =
                        ifelse(!is.null(heterogenous),
                               paste(heterogenous, collapse = " "), NA),
                      FE = ifelse(!is.null(FE),
                                  paste(FE, collapse = " "), "no"),
                      commaCLUSTER = ifelse(!is.null(cluster),
                                            paste(cluster, collapse = " "), "no"),
                      IPW = ifelse(!is.null(IPW),
                                   paste(IPW, collapse = " "), "no")),
       model_status = c(R = ifelse(status[1] ==
                                     1, "yes", "no"), S = ifelse(status[2] == 1, "yes", "no"),
                        P = ifelse(status[3] == 1, "yes", "no")))

}
