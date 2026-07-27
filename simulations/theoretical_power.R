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

prob_for_model <- function(model) {
  switch(model,
         logistic    = c(.5, .5),
         multinomial = rep(1 / 3, 3),
         ordinal     = rep(1 / 3, 3),
         NULL)
}

theoretical_power <- function(es, N, k, model, df_test = k) {
  es <- pmax(es, 0)
  if (model == "gaussian") {
    f2 <- es / (1 - es)
    pwr::pwr.f2.test(u = df_test, v = N - k - 1, f2 = f2, sig.level = .05)$power
  } else {
    gzlmpower::power.lrt(es = es, prob = prob_for_model(model), df = df_test, N = N)$power
  }
}

## Test df carried by the focal predictor alone (Study 1b design: x1 has a
## single slope in every model family except multinomial, where it has one
## slope per non-reference category -- yl-1 = 2 in this design's fixed yl=3).
focal_df <- function(model) if (model == "multinomial") 2 else 1
model_df <- function(model) if (model == "multinomial") 6 else 3
