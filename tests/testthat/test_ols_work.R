library(usefulr)
context("OLS regression with analyses function")

test_that("OLS regression with full functionality works", {
  expect_type(object =
                suppressWarnings(
                  usefulr::analyses(.DV = "mpg",
                                    .treat = "carb",
                                    .model = "lm",
                                    .covs = "disp",
                                    .heterogenous = "cyl",
                                    .IPW = "drat",
                                    .cluster = "gear",
                                    .subset = "qsec >= 15",
                                    .FE = "vs",
                                    .status = c(T,T,F),
                                    .data = mtcars,
                                    .margin_at = NULL,
                                    .treat_only = TRUE)),
              type = "list")

  expect_length(object =
                  suppressWarnings(
                    usefulr::analyses(.DV = "mpg",
                                      .treat = "carb",
                                      .model = "lm",
                                      .covs = "disp",
                                      .heterogenous = "cyl",
                                      .IPW = "drat",
                                      .cluster = "gear",
                                      .subset = "qsec >= 15",
                                      .FE = "vs",
                                      .status = c(T,T,F),
                                      .data = mtcars,
                                      .margin_at = NULL,
                                      .treat_only = TRUE)),
                n = 4)

})
