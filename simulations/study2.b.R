### get the required packages
if (!require(Rsimcity)) remotes::install_github("mcfanda/Rsimcity")
if (!require(gzlmpower)) remotes::install_github("mcfanda/gzlmpower")

library(future)
library(future.apply)

source(here::here("simulations/functions.R"))
### only the first predictor carries the true effect; the remaining k-1 predictors are
### rho-correlated nuisance predictors 


one_model<-function(eta2, N,  k, model, rho=.0) {
  
  beta<-make_beta(model,k)
  xcov<-make_xcov(k,rho)
  yl<-if (model %in% c("multinomial","ordinal")) 3 else NULL
  .data<-Rsimcity::sample_by_eta2(n = N, eta2 = eta2, model = model, k = k, beta = beta, yl = yl, x_cov = xcov)
  form<-as.formula(paste0("y~",paste0("x",1:k,collapse = "+")))
  if (model %in% c("logistic")) {
    if (length(unique(.data$y)) < 2) return(NULL)
    mod<-glm(form,data=.data,family=binomial())
    bad <- !mod$converged || any(abs(coef(mod)) > 10)
  }
  if (model %in% c("gaussian")) {
    mod<-lm(form,data=.data)
    bad <- FALSE
  }
  if (model %in% c("multinomial")) {
    if (length(unique(.data$y)) < yl) return(NULL)
    mod<-nnet::multinom(form,data=.data,model = T,trace = FALSE)
    bad <- mod$convergence != 0 || any(abs(coef(mod)) > 10)
  }
  if (model %in% c("ordinal"))  {
    if (length(unique(.data$y)) < yl) return(NULL)
    mod<-ordinal::clm(form,data=.data)
    bad <- mod$convergence$code != 0 || any(abs(coef(mod)) > 10)
  }
  
  if (bad) return(NULL)   # excluded: non-converged or separated fit, per paper criterion
  
  res<-gzlmpower::eta2(mod)
  anov<-attr(res,"test")
  pcol<-intersect(c("Pr(>Chisq)","Pr(>F)"), colnames(anov))
  p<-anov["x1",pcol]
  value<-res[1,1]
  avalue<-res[1,2]
  data.frame(eta2=eta2,N=N,k=k,model=model,eeta2=value,aeeta2=avalue, p=p)
}

future::plan(future::multisession)

### set search_rep=1000 for production
find_n <- function(eta2, k, model, rho = 0,search_rep=10) {
  found <- FALSE

  Nt <- round(gzlmpower::power.lrt(eta2, df = focal_df(model), power = .80, prob = prob_for_model(model))$N) 
  ### a fixed "-10" offset gives a large enough safety margin for small Nt but not
  ### for large Nt (small effect size): the power curve's noise floor at search_rep
  ### reps means a starting point too close to Nt can cross .80 on sheer sampling
  ### luck in a single batch, reporting a falsely small "required N". Scaling the
  ### offset proportionally to Nt keeps the starting true power comfortably below
  ### .80 regardless of Nt's magnitude.
  N<-round(Nt * 0.80)
  cpower<-.80
  while (!found) {
    trials <- future.apply::future_lapply(1:search_rep, function(i) one_model(eta2, k = k, N = N, model=model, rho=rho), future.seed = TRUE)
    trials <- trials[!vapply(trials, is.null, logical(1))]
    trials <- do.call(rbind,trials)
    ### a batch can come back empty (every replicate non-convergent/separated at a
    ### small trial N) -- treat that as power 0 rather than NaN, which would
    ### otherwise crash the `if (power>cpower)` check below.
    power <- if (is.null(trials) || nrow(trials) == 0) 0 else mean(as.numeric(trials$p<.05))
    if (power>cpower) {
      ### a single batch crossing .80 can be a lucky draw rather than the true
      ### crossover (confirmed empirically: "stop at first crossing" is an
      ### optional-stopping bias). Pool a much larger confirmation batch at this
      ### same N before accepting it, rather than trusting the first batch alone.
      confirm <- future.apply::future_lapply(1:(search_rep*5), function(i) one_model(eta2, k = k, N = N, model=model, rho=rho), future.seed = TRUE)
      confirm <- confirm[!vapply(confirm, is.null, logical(1))]
      confirm <- do.call(rbind, confirm)
      all_trials <- rbind(trials, confirm)
      power <- if (is.null(all_trials) || nrow(all_trials) == 0) 0 else mean(as.numeric(all_trials$p<.05))
      if (power>cpower) found <- TRUE
    }
    N<-N+1
  }
  data.frame(N=N, Nt=Nt)
}
# test
#find_n(r2, k, model, rho = 0,search_rep = 1000) 

runner<-Rsimcity::Runner$new("required N")
runner$parallel=FALSE
runner$step<-find_n
runner$params<-list(rho=.3,search_rep=10000,k=3)
runner$design<-list(eta2=seq(.025,.2,.025),model=c("logistic","multinomial","ordinal"))

results<-runner$experiment(Rep=1)

save(results,file=here::here("simulations/Data/study2.b.data.Rdata"))

