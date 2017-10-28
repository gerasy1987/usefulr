library(usefulr)
context("Probit regression with analyses function")

test_that("Probit regression with marginal effects works", {
  expect_warning(object = usefulr::analyses(.DV = "carb",
                                            .treat = "am",
                                            .model = "oprobit",
                                            .covs = "vs",
                                            .heterogenous = NULL,
                                            .cluster = "gear",
                                            .subset = "qsec >= 15",
                                            # .FE = "cyl",
                                            .status = c(T,T,F),
                                            .data = mtcars,
                                            .margin_at = 1,
                                            .IPW = NULL,
                                            .treat_only = TRUE),
              regexp = "Marginal effects for Logit and Probit models are only reported for .treat variable.")

  expect_type(object =
                suppressWarnings(
                  usefulr::analyses(.DV = "am",
                                    .treat = "vs",
                                    .model = "probit",
                                    .covs = "carb",
                                    .heterogenous = NULL,
                                    .cluster = "gear",
                                    .subset = "qsec >= 15",
                                    .FE = "cyl",
                                    .status = c(T,T,F),
                                    .data = mtcars,
                                    .margin_at = 1,
                                    .IPW = NULL,
                                    .treat_only = FALSE)),
              type = "list")

  expect_length(object =
                  suppressWarnings(
                    usefulr::analyses(.DV = "am",
                                      .treat = "vs",
                                      .model = "probit",
                                      .covs = "carb",
                                      .heterogenous = NULL,
                                      .cluster = "gear",
                                      .subset = "qsec >= 15",
                                      .FE = "cyl",
                                      .status = c(T,T,F),
                                      .data = mtcars,
                                      .margin_at = 1,
                                      .IPW = NULL,
                                      .treat_only = FALSE)),
                n = 4)

})

test_that("Probit regression with no marginal effects works", {
  expect_type(object = usefulr::analyses(.DV = "am",
                                         .treat = "cyl",
                                         .model = "probit",
                                         .covs = "carb",
                                         .heterogenous = NULL,
                                         .cluster = "gear",
                                         .subset = "qsec >= 15",
                                         .FE = "cyl",
                                         .status = c(T,T,F),
                                         .data = mtcars,
                                         .margin_at = NULL,
                                         .IPW = NULL,
                                         .treat_only = TRUE),
              type = "list")

  expect_length(object = usefulr::analyses(.DV = "am",
                                           .treat = "cyl",
                                           .model = "probit",
                                           .covs = "carb",
                                           .heterogenous = NULL,
                                           .cluster = "gear",
                                           .subset = "qsec >= 15",
                                           .FE = "cyl",
                                           .status = c(T,T,F),
                                           .data = mtcars,
                                           .margin_at = NULL,
                                           .IPW = NULL,
                                           .treat_only = TRUE),
                n = 4)

})
