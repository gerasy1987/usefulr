library(usefulr)
context("Analyses Functionality")

test_that("Analyses with Logit works", {
  expect_type(object = usefulr::analyses(DV = "am",
                                         treat = "vs",
                                         model = "logit",
                                         covs = "carb",
                                         heterogenous = NULL,
                                         cluster = "gear",
                                         subset = NULL,
                                         FE = NULL,
                                         status = c(T,T,F),
                                         data = mtcars,
                                         margin_at = "vs = 1",
                                         IPW = NULL),
              type = "list")

  expect_length(object = usefulr::analyses(DV = "am",
                                           treat = "vs",
                                           model = "logit",
                                           covs = "carb",
                                           heterogenous = NULL,
                                           cluster = "gear",
                                           subset = NULL,
                                           FE = NULL,
                                           status = c(T,T,F),
                                           data = mtcars,
                                           margin_at = "vs = 1",
                                           IPW = NULL),
                n = 4)

})

test_that("Analyses with Probit works", {
  expect_type(object = usefulr::analyses(DV = "am",
                                         treat = "vs",
                                         model = "probit",
                                         covs = "carb",
                                         heterogenous = NULL,
                                         cluster = "gear",
                                         subset = NULL,
                                         FE = NULL,
                                         status = c(T,T,F),
                                         data = mtcars,
                                         margin_at = "vs = 1",
                                         IPW = NULL),
              type = "list")

  expect_length(object = usefulr::analyses(DV = "am",
                                           treat = "vs",
                                           model = "probit",
                                           covs = "carb",
                                           heterogenous = NULL,
                                           cluster = "gear",
                                           subset = NULL,
                                           FE = NULL,
                                           status = c(T,T,F),
                                           data = mtcars,
                                           margin_at = "vs = 1",
                                           IPW = NULL),
                n = 4)

})

test_that("Analyses with Linear works", {
  expect_type(object = usefulr::analyses(DV = "am",
                                         treat = "vs",
                                         model = "lm",
                                         covs = "carb",
                                         heterogenous = NULL,
                                         cluster = "gear",
                                         subset = NULL,
                                         FE = NULL,
                                         status = c(T,T,F),
                                         data = mtcars,
                                         margin_at = "vs = 1",
                                         IPW = NULL),
              type = "list")

  expect_length(object = usefulr::analyses(DV = "am",
                                           treat = "vs",
                                           model = "lm",
                                           covs = "carb",
                                           heterogenous = NULL,
                                           cluster = "gear",
                                           subset = NULL,
                                           FE = NULL,
                                           status = c(T,T,F),
                                           data = mtcars,
                                           margin_at = "vs = 1",
                                           IPW = NULL),
                n = 4)

})

