library(usefulr)
context("Logit regression with analyses function")

test_that("Logit regression with marginal effects works", {
  expect_warning(object = usefulr::analyses(.DV = "am",
                                            .treat = "vs",
                                            .model = "logit",
                                            .covs = "carb",
                                            .heterogenous = NULL,
                                            .cluster = "gear",
                                            .subset = "qsec >= 15",
                                            .FE = "cyl",
                                            .status = c(T,T,F),
                                            .data = mtcars,
                                            .margin_at = 1,
                                            .IPW = NULL,
                                            .treat_only = FALSE),
              regexp = 'Marginal effects for Logit and Probit models are only reported for .treat variable.')

  expect_type(object =
                  suppressWarnings(
                    usefulr::analyses(.DV = "am",
                                      .treat = "vs",
                                      .model = "logit",
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
                                      .model = "logit",
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

test_that("Logit regression with no marginal effects works", {
  expect_type(object = usefulr::analyses(.DV = "am",
                                         .treat = "vs",
                                         .model = "logit",
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
                                           .treat = "vs",
                                           .model = "logit",
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
