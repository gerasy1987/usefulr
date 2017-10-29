#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param w DESCRIPTION.
#' @param rev.dum DESCRIPTION.
#' @param vcov DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
#'
ocme_mod <- function(w,
                     rev.dum = TRUE,
                     vcov = NULL) {

  requireNamespace("MASS", quietly = TRUE)

  if (!inherits(w, "polr")) {
    stop("Need an ordered choice model from 'polr()'.\n")
  }
  if (w$method != "probit" & w$method != "logistic") {
    stop("Need a probit or logit model.\n")
  }
  lev <- w$lev
  J <- length(lev)
  # x.name <- attr(x = w$terms, which = "term.labels")
  # x2 <- w$model[, x.name]
  # colnames(x2) <- c("vs", "carb")
  # # ww <- paste("~ -1", paste("+", x.name, collapse = " "), collapse = " ")
  x <- stats::model.matrix(w)
  x.name <- colnames(x)[-1]
  x.bar <- as.matrix(colMeans(x))
  b.est <- as.matrix(stats::coef(w))

  x.bar <- as.matrix(x.bar[rownames(x.bar) %in% rownames(b.est),1])

  K <- nrow(b.est)
  xb <- as.vector(t(x.bar) %*% b.est)
  z <- c(-10^6, w$zeta, 10^6)
  pfun <- switch(w$method, probit = pnorm, logistic = plogis)
  dfun <- switch(w$method, probit = dnorm, logistic = dlogis)

  if (is.null(vcov)) {
    V2 <- vcov(w)
  } else {
    if (any(dim(vcov(w)) != dim(vcov))) stop("Dimensions of custom vcov and model vcov do not match.\n")
    V2 <- vcov
  }

  V3 <- rbind(cbind(V2, 0, 0), 0, 0)
  ind <- c(1:K, nrow(V3) - 1, (K + 1):(K + J - 1), nrow(V3))
  V4 <- V3[ind, ]
  V5 <- V4[, ind]
  f.xb <- dfun(z[1:J] - xb) - dfun(z[2:(J + 1)] - xb)
  me <- b.est %*% matrix(data = f.xb, nrow = 1)
  colnames(me) <- paste("effect", lev, sep = ".")
  se <- matrix(0, nrow = K, ncol = J)
  for (j in 1:J) {
    u1 <- c(z[j] - xb)
    u2 <- c(z[j + 1] - xb)
    if (w$method == "probit") {
      s1 <- -u1
      s2 <- -u2
    }
    else {
      s1 <- 1 - 2 * pfun(u1)
      s2 <- 1 - 2 * pfun(u2)
    }
    d1 <- dfun(u1) * (diag(1, K, K) - s1 * (b.est %*% t(x.bar)))
    d2 <- -1 * dfun(u2) * (diag(1, K, K) - s2 * (b.est %*%
                                                   t(x.bar)))
    q1 <- dfun(u1) * s1 * b.est
    q2 <- -1 * dfun(u2) * s2 * b.est
    dr <- cbind(d1 + d2, q1, q2)
    V <- V5[c(1:K, K + j, K + j + 1), c(1:K, K + j, K +
                                          j + 1)]
    cova <- dr %*% V %*% t(dr)
    se[, j] <- sqrt(diag(cova))
  }
  colnames(se) <- paste("SE", lev, sep = ".")
  rownames(se) <- x.name
  if (rev.dum) {
    for (k in 1:K) {
      if (identical(sort(unique(x[, k])), c(0, 1))) {
        for (j in 1:J) {
          x.d1 <- x.bar
          x.d1[k, 1] <- 1
          x.d0 <- x.bar
          x.d0[k, 1] <- 0
          ua1 <- z[j] - t(x.d1) %*% b.est
          ub1 <- z[j + 1] - t(x.d1) %*% b.est
          ua0 <- z[j] - t(x.d0) %*% b.est
          ub0 <- z[j + 1] - t(x.d0) %*% b.est
          me[k, j] <- pfun(ub1) - pfun(ua1) - (pfun(ub0) -
                                                 pfun(ua0))
          d1 <- (dfun(ua1) - dfun(ub1)) %*% t(x.d1) -
            (dfun(ua0) - dfun(ub0)) %*% t(x.d0)
          q1 <- -dfun(ua1) + dfun(ua0)
          q2 <- dfun(ub1) - dfun(ub0)
          dr <- cbind(d1, q1, q2)
          V <- V5[c(1:K, K + j, K + j + 1), c(1:K, K +
                                                j, K + j + 1)]
          se[k, j] <- sqrt(c(dr %*% V %*% t(dr)))
        }
      }
    }
  }
  t.value <- me/se
  p.value <- 2 * (1 - pt(abs(t.value), w$df.residual))
  out <- list()
  for (j in 1:J) {
    out[[j]] <- data.frame(term = x.name,
                           estimate = me[, j], std.error = se[,j],
                           t.value = t.value[, j], p.value = p.value[,j])
  }
  out[[J + 1]] <- me
  names(out) <- paste("ME", c(lev, "all"), sep = ".")
  result <- listn(w, out)
  return(result)
}
