# display labels for each type of model
model_labels <- c(logistic = "Logistic", multinomial = "Multinomial",
                   ordinal = "Ordinal", gaussian = "Gaussian")

# compute the expected probabilities for each type of model

prob_for_model <- function(model) {
  switch(model,
         logistic    = .5,
         multinomial = rep(1 / 3, 3),
         ordinal     = rep(1 / 3, 3),
         NULL)
}
# prepare betas for each type of model
make_beta<-function(model, k, yl=3, others=0) {
  if (model=="multinomial") {
    B<-matrix(0,nrow=k,ncol=yl-1)
    B[1,]<-1
    B
  } else {
    b<-rep(others,k)
    b[1]<-1
    b
  }
}
# prepare the X covariance matrix
make_xcov<-function(k, rho) {
  S<-matrix(rho,k,k); diag(S)<-1; S
}

focal_df <- function(model) if (model == "multinomial") 2 else 1
model_df <- function(model) if (model == "multinomial") 6 else 3

## Closed-form ("theoretical") power for a design cell, computed by plugging a
## chosen effect-size estimate (e.g. the cell's mean raw or adjusted R^2/eta^2)
## into the appropriate power formula for the model family: the classical
## F-test route (pwr::pwr.f2.test) for the gaussian benchmark, and the LRT
## route (gzlmpower::power.lrt) for the categorical models, using each model's
## default outcome-category distribution from the simulation design.
##
## `df_test` is the numerator/test degrees of freedom for whatever is being
## tested -- the whole model (Study 1a, R^2: all k predictors, so it defaults
## to k) or a single focal predictor (Study 1b, eta^2: 1 df, except for the
## multinomial model, where the focal predictor carries yl-1 slopes). `k`
## itself is still needed even in the single-predictor case, since it sets the
## Gaussian F-test's denominator df (all k predictors are in the fitted model).
theoretical_power <- function(es, N, k, model, df_test = k) {
  es <- pmax(es, 0)
  if (model == "gaussian") {
    f2 <- es / (1 - es)
    pwr::pwr.f2.test(u = df_test, v = N - k - 1, f2 = f2, sig.level = .05)$power
  } else {
    gzlmpower::power.lrt(es = es, prob = prob_for_model(model), df = df_test, N = N)$power
  }
}

## NOT WIRED UP ANYWHERE YET -- prototype only, for inspection before it
## replaces MBESS::conf.limits.nc.chisq() inside gzlmpower's ci_eta2()/ci_eta2p().
##
## Base-R replacement for MBESS::conf.limits.nc.chisq(): a (1-alpha) CI for the
## noncentrality parameter of a chi-square statistic, via Steiger's (2004)
## inversion of the noncentral chi-square CDF -- pchisq(Chi.Square, df, ncp)
## is strictly decreasing in ncp, so each limit is just a uniroot() call
## against base R's own noncentral-chi-square CDF; nothing MBESS-specific is
## actually needed. Same argument names and Lower.Limit/Upper.Limit return
## shape as MBESS::conf.limits.nc.chisq(), so it's a drop-in for the two call
## sites in gzlmpower/R/CI.R once validated.
##
## Boundary case: pchisq(Chi.Square, df, ncp = 0) is the supremum of the CDF
## over ncp >= 0 (increasing ncp only shifts probability mass up, lowering the
## CDF at a fixed Chi.Square). If even ncp = 0 can't reach the target
## probability, no valid ncp >= 0 does either, and NA is returned -- matching
## MBESS's own NULL/NA behavior at that boundary, which gzlmpower's ci_eta2()
## already clips to 0 downstream.
conf_limits_nc_chisq <- function(Chi.Square, df, conf.level = 0.95) {
  alpha <- 1 - conf.level
  cdf_at <- function(ncp) stats::pchisq(Chi.Square, df = df, ncp = ncp)

  solve_ncp <- function(target) {
    if (cdf_at(0) <= target) return(NA_real_)
    stats::uniroot(function(ncp) cdf_at(ncp) - target,
                    lower = 0, upper = 1, extendInt = "downX")$root
  }

  list(
    Lower.Limit = solve_ncp(1 - alpha / 2),
    Upper.Limit = solve_ncp(alpha / 2)
  )
}
