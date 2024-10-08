% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_analyses.R
\name{make_analyses}
\alias{make_analyses}
\title{Estimation Function}
\usage{
make_analyses(
  dv,
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
  round_digits = 3,
  return_df = FALSE,
  ...
)
}
\arguments{
\item{dv}{Dependent variable specified as character.}

\item{treat}{Treatment vector of variables specified as character vector.}

\item{covs}{Character vector of covariates specified as character vector.}

\item{heterogenous}{Character vector of covariates to interact with treatments specified as character vector.}

\item{FE}{Character vector of fixed effects covariates specified as character vector.}

\item{IPW}{Inverse probability weights specified as character vector.}

\item{IV_list}{Character string. Should be a character string which presents valid IV formula as specified in \code{lfe::felm}.}

\item{cluster}{Covariate for clustered robust standard errors as defined by \code{multiwayvcov::cluster.vcov} function. Currently only one clustering variable option supported specified as character vector.}

\item{robust}{Logical. Whether to report heteroskedastic robust standard errors. Implemented only for linear models for now.}

\item{data}{Data frame which contains all the relevant variables.}

\item{subset}{character string specifing logical expression for subsetting.}

\item{estimator}{Function that takes all arguments above, \code{fit_formula} and \code{frame_df} as arguments and returns a tibble with the following columns: "term" (required), "estimate" (required), "std.error" (optional), "p.value" (required).}

\item{estimator_name}{Character string giving name of the estimator used}

\item{treat_only}{TRUE/FALSE specifying whether only estimates directly related to \code{treat} should be reported (including heterogeneity variables). Defaults to \code{FALSE}.}

\item{status}{Logical vector of length 3, specifying whether the model was pre-(R)egistered, run in (S)cript and reported in (P)aper respectively.}

\item{round_digits}{Integer. How many decimal points to round to in the output.}

\item{return_df}{If \code{TRUE} dataframe used for estimation will be returned.}

\item{...}{Other parameters used in estimate function}
}
\value{
List of three objects. \code{estimates} is estimates from the model and corresponding standard errors. \code{stat} is vector of adjusted R squared and number of observations. \code{model_spec} is logical vector of characteristics of the model.
}
\description{
Estimation Function
}
