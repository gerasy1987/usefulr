# Package For Regression and Output Functions for Replications

## Functionality

### `logit` and `probit` models

- Here is the example of call to estimate and produce the output for `probit` model with binary treatment, clustered SE's and McFadden R2. The output only reports coefficient estimates for treatment variable and no significance stars.

```{r}

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
                  treat_only = TRUE,
                  r2_glm = "McFadden",
                  stars = FALSE)
```

- Here is the example of call to estimate and produce the output for `logit` model with binary treatment on subsetted dataset with clustered SE's, fixed effects and McFadden R2. The output reports all coefficient estimates (except fixed effects) for treatment variable and significance stars.

```{r}
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
                  treat_only = FALSE,
                  IPW = NULL,
                  stars = TRUE)

```
