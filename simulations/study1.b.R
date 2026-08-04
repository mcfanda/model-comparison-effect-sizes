### get the required packages
library(Rsimcity)
library(gzlmpower)

source(here::here("simulations/functions.R"))
### only the first predictor carries the true effect; the remaining k-1 predictors are
### rho-correlated nuisance predictors -- same population design as before, expressed in
### Rsimcity::sample_by_eta2()'s more general (beta/x_cov) interface

### generate a sample given Eta-squared
one_model<-function(eta2, N,  k, model, rho=.0) {

    beta<-make_beta(model,k)
    xcov<-make_xcov(k,rho)
    yl<-if (model %in% c("multinomial","ordinal")) 3 else NULL
    .data<-Rsimcity::sample_by_eta2(n = N, eta2 = eta2, model = model, k = k, beta = beta, yl = yl, x_cov = xcov)
      form<-as.formula(paste0("y~",paste0("x",1:k,collapse = "+")))
      if (model %in% c("logistic")) {
        mod<-glm(form,data=.data,family=binomial())
        bad <- !mod$converged || any(abs(coef(mod)) > 10)
      }
      if (model %in% c("gaussian")) {
        mod<-lm(form,data=.data)
        bad <- FALSE
      }
      if (model %in% c("multinomial")) {
        mod<-nnet::multinom(form,data=.data,model = T,trace = FALSE)
        bad <- mod$convergence != 0 || any(abs(coef(mod)) > 10)
      }
      if (model %in% c("ordinal"))  {
        mod<-ordinal::clm(form,data=.data)
        bad <- mod$convergence$code != 0 || any(abs(mod$beta) > 10)
      }

      if (bad) return(NULL)   # excluded: non-converged or separated fit, per paper criterion

      res<-gzlmpower::eta2(mod,test=T,ci=T,quite=T)
      anov<-res$anova
      pcol<-intersect(c("Pr(>Chisq)","Pr(>F)"), colnames(anov))
      p<-anov["x1",pcol]
      value<-res$indices["x1","Eta_squared"]
      avalue<-res$indices["x1","Epsilon_squared"]
      cilo<-res$ci["x1","lower"]
      ciup<-res$ci["x1","upper"]
     data.frame(eta2=eta2,N=N,k=k,model=model,eeta2=value,aeeta2=avalue,
                cilo=cilo,ciup=ciup,
                p=p,trunc=as.numeric((avalue < 0)))
}

agg_fun<-function(data) {
   eta2<-as.numeric(data$eta2)
   eeta2<-as.numeric(data$eeta2)
   aeeta2<-as.numeric(data$aeeta2)
   meeta2<-mean(eeta2)
   maeeta2<-mean(aeeta2)
   bias<-mean(eeta2-eta2)
   rmse<-sqrt(mean((eeta2-eta2)^2))
   abias<-mean(aeeta2-eta2)
   armse<-sqrt(mean((aeeta2-eta2)^2))
   power<-mean(as.numeric(data$p<.05))
   ptrunc<-mean(as.numeric(data$trunc))
   covered <- mean(as.numeric((data$cilo <= eta2) & (eta2 <= data$ciup)))
   n_ok<-nrow(data)
   n_fail<-length(attr(data,"fail"))
   miss_lo <- mean(data$ciup < eta2)
   miss_hi <- mean(data$cilo >  eta2)
   data.frame(eeta2=meeta2,aeeta2=maeeta2,bias=bias,
              rmse=rmse,abias=abias,armse=armse,
              power=power,
              n_ok=n_ok,n_fail=n_fail,p_trunc=ptrunc,
              covered=covered,miss_lo=miss_lo,miss_hi=miss_hi)

}


runner<-Rsimcity::Runner$new("eta")
runner$parallel=TRUE
runner$par_method="multicore"
runner$step<-one_model
runner$aggregate<-agg_fun

##### use this to test the sim ###########
#runner$params<-list(rho=.3,eta2=0,N=50,model="logistic",k=3)
#runner$test_one()
#runner$test_aggregates()
######    ----   #########

runner$params<-list(rho=.3)
runner$design<-list(eta2=seq(0,.3,.03),N=c(30,60,90,120),k=c(3,5),model=c("gaussian", "logistic","multinomial","ordinal"))

results<-runner$experiment(Rep=5000)

save(results,file=here::here("simulations/Data/study1.b.data.Rdata"))
