% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stata_sum.R
\name{stata_sum}
\alias{stata_sum}
\title{Function to produce Stata-like variable(s) summary}
\usage{
stata_sum(.vars, .data, .percentile = c(0.25, 0.75), .by = NULL)
}
\arguments{
\item{.vars}{Character vector giving variables to be summarized}

\item{.data}{Data frame to be used for summary statistics construction}

\item{.percentile}{Numeric vector of length 2 giving two percentile levels to be produced. Both values in the vector should be between 0 and 1}

\item{.by}{Charatcer string giving variable to be used to break each variable summary by}
}
\value{
tibble object.
}
\description{
Produce variable(s) summary statistics
}
