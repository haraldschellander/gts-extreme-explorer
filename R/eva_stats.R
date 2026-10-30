# R/eva_stats.R
# Block-maxima extreme value analysis (GEV fitted by maximum likelihood).

library(extRemes)

MIN_ANNUAL_MAXIMA <- 15

fit_gev <- function(x) {
  fit <- extRemes::fevd(x, type = "GEV", method = "GMLE")
  par <- fit$results$par
  list(fit = fit, shape = unname(par["shape"]), params = par, n = length(x))
}

# Return levels with approximate 95% confidence intervals (normal approximation).
return_levels <- function(fit, return_period) {
  est <- as.numeric(extRemes::return.level(fit, return.period = return_period))
  lower <- upper <- rep(NA_real_, length(return_period))
  ci <- tryCatch(
    suppressWarnings(extRemes::ci.fevd(
      fit,
      alpha = 0.05,
      type = "return.level",
      return.period = return_period
    )),
    error = function(e) NULL
  )
  if (!is.null(ci)) {
    m <- tryCatch(matrix(as.numeric(ci), ncol = 3), error = function(e) NULL)
    if (!is.null(m) && nrow(m) == length(return_period)) {
      lower <- m[, 1]
      upper <- m[, 3]
    }
  }
  fin <- function(v) ifelse(is.finite(v), v, NA_real_)
  data.frame(
    period = return_period,
    level = est,
    lower = fin(lower),
    upper = fin(upper)
  )
}

# Empirical return periods (Gringorten plotting positions) for the data points.
empirical_levels <- function(x) {
  x <- sort(x)
  n <- length(x)
  p <- (seq_len(n) - 0.44) / (n + 0.12)
  data.frame(period = 1 / (1 - p), value = x)
}

tail_label <- function(shape) {
  if (is.na(shape)) {
    "Not available"
  } else if (shape < -0.05) {
    "Bounded upper tail"
  } else if (shape > 0.05) {
    "Heavy upper tail"
  } else {
    "Close to Gumbel"
  }
}
