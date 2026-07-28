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
make_beta<-function(model, k, yl=3) {
  if (model=="multinomial") {
    B<-matrix(0,nrow=k,ncol=yl-1)
    B[1,]<-1
    B
  } else {
    b<-rep(0,k)
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
