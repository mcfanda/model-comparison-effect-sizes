# ------------------------------------------------------------------------
# Independent verification of the Gaussian eta^2 simulation results in
# study1.b.data.Rdata, WITHOUT using Rsimcity or gzlmpower at all.
#
# Design mirrors study1.b.R exactly (focal predictor x1 has the only true
# effect; x2..xk are rho-equicorrelated nuisance predictors with zero true
# coefficient; error_sd = 1, matching Rsimcity's default), but:
#   - the population coefficient b1 needed to hit a target eta^2 is derived
#     in closed form from multivariate-normal theory (no Gauss-Hermite
#     quadrature at all -- for the Gaussian model this can be done exactly);
#   - eta^2 / epsilon^2 are computed from scratch via explicit full-vs-reduced
#     model SS, not via gzlmpower::eta2().
#
# Closed-form derivation:
#   X ~ N(0, Sigma), Sigma equicorrelated with corr rho, k variables.
#   y = b1*x1 + e,  e ~ N(0, error_sd^2), independent of X.
#   R1bar2 = pop. R^2 of x1 regressed on the other (k-1) equicorrelated
#            predictors = rho^2*(k-1) / (1 + (k-2)*rho)
#   population SST    = b1^2 + error_sd^2
#   population SS_x1  = b1^2 * (1 - R1bar2)
#   population eta2   = SS_x1 / SST = b1^2*(1-R1bar2) / (b1^2+error_sd^2)
#   => b1^2 = eta2 * error_sd^2 / (1 - R1bar2 - eta2)
# ------------------------------------------------------------------------

r1bar2 <- function(k, rho) {
  m <- k - 1
  rho^2 * m / (1 + (m - 1) * rho)
}

b1_for_eta2 <- function(eta2, k, rho, error_sd = 1) {
  R1b2 <- r1bar2(k, rho)
  stopifnot(eta2 < 1 - R1b2)  # otherwise no finite b1 achieves this eta2
  sqrt(eta2 * error_sd^2 / (1 - R1b2 - eta2))
}

one_rep <- function(N, k, b1, rho, error_sd = 1) {
  Sigma <- matrix(rho, k, k); diag(Sigma) <- 1
  Z <- matrix(rnorm(N * k), N, k)
  X <- Z %*% chol(Sigma)
  y <- b1 * X[, 1] + rnorm(N, 0, error_sd)
  d <- as.data.frame(X); names(d) <- paste0("x", 1:k); d$y <- y

  form_full <- as.formula(paste0("y~", paste0("x", 1:k, collapse = "+")))
  form_red  <- as.formula(paste0("y~", paste0("x", 2:k, collapse = "+")))

  mod_full <- lm(form_full, data = d)
  mod_red  <- lm(form_red,  data = d)

  sse_full <- sum(residuals(mod_full)^2)
  sse_red  <- sum(residuals(mod_red)^2)
  sst      <- sum((y - mean(y))^2)

  ss_x1    <- sse_red - sse_full
  mse_full <- sse_full / mod_full$df.residual

  raw <- ss_x1 / sst
  adj <- max(0, (ss_x1 - 1 * mse_full) / sst)
  c(raw = raw, adj = adj)
}

sim_gaussian_eta2 <- function(N, k, eta2_target, rho = .3, error_sd = 1, nrep = 5000) {
  b1 <- b1_for_eta2(eta2_target, k, rho, error_sd)
  out <- t(replicate(nrep, one_rep(N, k, b1, rho, error_sd)))
  data.frame(eta2 = eta2_target, N = N, k = k,
             raw_mean = mean(out[, "raw"]), adj_mean = mean(out[, "adj"]),
             raw_bias = mean(out[, "raw"]) - eta2_target,
             adj_bias = mean(out[, "adj"]) - eta2_target)
}

set.seed(20260728)
grid <- expand.grid(N = c(30, 60, 90, 120), k = c(3, 5), eta2 = seq(.03, .30, .03))

cat("Running independent (non-quadrature, non-package) Gaussian eta2 check...\n")
res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  sim_gaussian_eta2(N = g$N, k = g$k, eta2_target = g$eta2, nrep = 5000)
}))
rownames(res) <- NULL

save(res, file = here::here("simulations/checks/gaussian_eta2_independent_check.Rdata"))

cat("\n=== Independent check: N=30, both k ===\n")
print(res[res$N == 30, ], digits = 3, row.names = FALSE)
