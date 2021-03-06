% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/absorb.R
\name{absorb}
\alias{absorb}
\title{Function to pass to lfe::getfe() function to return intercept as in arge function in Stata}
\usage{
absorb(gamma, addnames, .FE)
}
\arguments{
\item{gamma}{A numeric vector of FE estimates from lfe::felm().}

\item{addnames}{A character vector of FE level names.}

\item{.FE}{A numeric vector of FE from the data.}
}
\value{
Returns scalar vector of length one.
}
\description{
Function to pass to lfe::getfe() function to return intercept as in arge function in Stata
}
\examples{
\dontrun{icpt <- unname(
                   plyr::name_rows(
                   lfe::getfe(fit, ef = absorb, se = T, bN = 1000)
                   )
                 )}

}
