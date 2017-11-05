Something fascinating
================
Georgiy Syunyaev
2017-11-05

``` r
pacman::p_load(knitr)

opts_chunk$set(include = TRUE, 
               results = "markup", 
               message = FALSE, 
               warning = FALSE, 
               error   = FALSE)
```

`usefulr::analyses()` functionality
===================================

Load the package
----------------

``` r
pacman::p_load_gh("gerasy1987/usefulr")
```

Linear models (`model = "lm"`)
------------------------------

-   Here is the example of call to estimate and produce the output for
    `lm` (OLS) model with binary treatment, clustered SE’s and fixed
    effects. The output reports all coefficient estimates for treatment
    variable and no significance stars.

``` r
usefulr::analyses(DV = "gear",
                   treat = "vs",
                   model = "lm",
                   covs = NULL,
                   heterogenous = NULL,
                   cluster = "am",
                   subset = NULL,
                   FE = "carb",
                   status = c(T,T,F),
                   data = mtcars,
                   IPW = NULL,
                   treat_only = FALSE,
                   stars = FALSE)
```

    ## Call:
    ## usefulr::analyses(DV = "gear", treat = "vs", covs = NULL, heterogenous = NULL, 
    ##     subset = NULL, FE = "carb", cluster = "am", IPW = NULL, data = mtcars, 
    ##     model = "lm", treat_only = FALSE, status = c(T, T, F), stars = FALSE)
    ## 
    ## Estimation formula:
    ## [1] "gear ~ vs | carb | 0 | am"
    ## 
    ## Estimates:
    ##   term estimate std.error
    ## 1   vs    0.683     0.546
    ## 
    ## Summary:
    ## Adj. R2 = 0.280 , N = 32

-   Here is the example of call to estimate and produce the output for
    `lm` (OLS) model with continous treatment on subsetted dataset with
    clustered SE’s and fixed effects. The output reports coefficient
    estimates for treatment variable and significance stars.

``` r
usefulr::analyses(DV = "mpg",
                  treat = "cyl",
                  model = "lm",
                  covs = "carb",
                  heterogenous = "hp",
                  cluster = "gear",
                  subset = "disp >= 0 & disp <= 300",
                  FE = "vs",
                  status = c(T,T,F),
                  data = mtcars,
                  treat_only = TRUE,
                  stars = TRUE)
```

    ## Call:
    ## usefulr::analyses(DV = "mpg", treat = "cyl", covs = "carb", heterogenous = "hp", 
    ##     subset = "disp >= 0 & disp <= 300", FE = "vs", cluster = "gear", 
    ##     data = mtcars, model = "lm", treat_only = TRUE, status = c(T, 
    ##         T, F), stars = TRUE)
    ## 
    ## Estimation formula:
    ## [1] "mpg ~ cyl + cyl:hp + hp + carb | vs | 0 | gear"
    ## 
    ## Estimates:
    ##     term estimate std.error
    ## 1    cyl   -5.172     0.933
    ## 2 cyl:hp    0.026     0.006
    ## 
    ## Summary:
    ## Adj. R2 = 0.621 , N = 21

Binary outcomes models (`model = "logit"` or `model = "probit"`)
----------------------------------------------------------------

-   Here is the example of call to estimate and produce the output for
    `probit` model with binary treatment, clustered SE’s. The output
    reports Adjusted McFadden R2, coefficient estimates only for
    treatment variable and no significance stars in printout.

``` r
usefulr::analyses(DV = "am",
                  treat = "vs",
                  model = "probit",
                  covs = "carb",
                  heterogenous = NULL,
                  cluster = "gear",
                  subset = NULL,
                  FE = NULL,
                  status = c(T,T,F),
                  data = mtcars,
                  IPW = NULL,
                  margin_at = NULL,
                  treat_only = TRUE,
                  stars = FALSE)
```

    ## Call:
    ## usefulr::analyses(DV = "am", treat = "vs", covs = "carb", heterogenous = NULL, 
    ##     subset = NULL, FE = NULL, cluster = "gear", IPW = NULL, data = mtcars, 
    ##     model = "probit", treat_only = TRUE, margin_at = NULL, status = c(T, 
    ##         T, F), stars = FALSE)
    ## 
    ## Estimation formula:
    ## [1] "am ~ vs + carb"
    ## 
    ## Estimates:
    ##   term estimate std.error
    ## 1   vs    0.788     0.963
    ## 
    ## Summary:
    ## Adj. R2 = -0.044 , N = 32

-   Here is the example of call to estimate and produce the output for
    `logit` model with binary treatment on subsetted dataset with
    clustered SE’s and fixed effects. The output reports Adjusted
    McFadden R2, estimates and SE’s for average marginal effects for all
    independent variables (be careful with heterogenous marginal effects
    in non-linear models) and significance stars in printout.

``` r
usefulr::analyses(DV = "am",
                  treat = "vs",
                  model = "logit",
                  covs = NULL,
                  heterogenous = NULL,
                  cluster = "gear",
                  subset = "disp >= 0 & disp <= 300",
                  FE = "gear",
                  status = c(F,F,F),
                  data = mtcars,
                  margin_at = TRUE,
                  treat_only = FALSE,
                  IPW = NULL,
                  stars = TRUE)
```

    ## Call:
    ## usefulr::analyses(DV = "am", treat = "vs", covs = NULL, heterogenous = NULL, 
    ##     subset = "disp >= 0 & disp <= 300", FE = "gear", cluster = "gear", 
    ##     IPW = NULL, data = mtcars, model = "logit", treat_only = FALSE, 
    ##     margin_at = TRUE, status = c(F, F, F), stars = TRUE)
    ## 
    ## Estimation formula:
    ## [1] "am ~ vs + factor(gear)"
    ## 
    ## Estimates:
    ##   term estimate std.error
    ## 1   vs   -2.122     0.152
    ## 
    ## Summary:
    ## Adj. R2 = 0.330 , N = 21

Ordered categorical outcomes (`model = "ologit"` or `model = "oprobit"`)
------------------------------------------------------------------------

-   Here is the example of call to estimate and produce the output for
    `ologit` (ordered logit) model with clustered SE’s and fixed
    effects. The output reports Adjusted McFadden R2, estimates and SE’s
    of coefficients for all independent variables and no significance
    stars.

``` r
usefulr::analyses(DV = "gear",
                  treat = "vs",
                  model = "ologit",
                  covs = NULL,
                  heterogenous = NULL,
                  cluster = "am",
                  subset = NULL,
                  FE = "am",
                  status = c(T,T,F),
                  data = mtcars,
                  margin_at = NULL,
                  IPW = NULL,
                  treat_only = FALSE,
                  stars = FALSE)
```

    ## Call:
    ## usefulr::analyses(DV = "gear", treat = "vs", covs = NULL, heterogenous = NULL, 
    ##     subset = NULL, FE = "am", cluster = "am", IPW = NULL, data = mtcars, 
    ##     model = "ologit", treat_only = FALSE, margin_at = NULL, status = c(T, 
    ##         T, F), stars = FALSE)
    ## 
    ## Estimation formula:
    ## [1] "gear ~ vs + factor(am)"
    ## 
    ## Estimates:
    ##   term estimate std.error
    ## 1   vs    0.552     2.794
    ## 
    ## Summary:
    ## Adj. R2 = 0.377 , N = 32

-   Here is the example of call to estimate and produce the output for
    `oprobit` (ordered probit) model with clustered SE’s, fixed effects
    and McFadden R2. The output reports Adjusted McFadden R2, estimates
    of average marginal effects for treatment variable only on being in
    category `"5"` of the outcome and significance stars.

``` r
usefulr::analyses(DV = "gear",
                  treat = "vs",
                  model = "oprobit",
                  covs = NULL,
                  heterogenous = NULL,
                  cluster = "am",
                  subset = NULL,
                  FE = NULL,
                  status = c(T,T,F),
                  data = mtcars,
                  margin_at = "5",
                  IPW = NULL,
                  treat_only = TRUE,
                  stars = TRUE)
```

    ## Call:
    ## usefulr::analyses(DV = "gear", treat = "vs", covs = NULL, heterogenous = NULL, 
    ##     subset = NULL, FE = NULL, cluster = "am", IPW = NULL, data = mtcars, 
    ##     model = "oprobit", treat_only = TRUE, margin_at = "5", status = c(T, 
    ##         T, F), stars = TRUE)
    ## 
    ## Estimation formula:
    ## [1] "gear ~ vs"
    ## 
    ## Estimates:
    ##   term estimate std.error
    ## 1   vs    0.121     0.106
    ## 
    ## Summary:
    ## Adj. R2 = -0.005 , N = 32
