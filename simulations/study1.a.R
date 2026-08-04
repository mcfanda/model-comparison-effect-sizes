### get the required packages
library(Rsimcity)
library(gzlmpower)

source(here::here("simulations/functions.R"))
### only the first predictor carries the true effect; the remaining k-1 predictors are
### rho-correlated nuisance predictors 

### generate a sample given R-squared
one_model<-function(r2, N,  k, model, rho=.0) {

      beta<-make_beta(model,k)
      xcov<-make_xcov(k,rho)
      yl<-if (model %in% c("multinomial","ordinal")) 3 else NULL
      .data<-Rsimcity::sample_by_r2(n = N, R2 = r2, model = model, k = k, beta = beta, yl = yl, x_cov = xcov)
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

      res<-gzlmpower::r2(mod,test=T,ci=T)
      data.frame(r2=r2,N=N,k=k,model=model,
                 er2=res$indices$r2,aer2=res$indices$r2adj,
                 cilo=res$ci$lower,ciup=res$ci$upper, 
                 test=res$anova$test,p=res$anova$p,trunc=as.numeric((res$indices$r2adj < 0)))
}

agg_fun<-function(data) {
   r2<-as.numeric(data$r2)
   er2<-as.numeric(data$er2)
   aer2<-as.numeric(data$aer2)
   mer2<-mean(er2)
   maer2<-mean(aer2)
   bias<-mean(er2-r2)
   rmse<-sqrt(mean((er2-r2)^2))
   abias<-mean(aer2-r2)
   armse<-sqrt(mean((aer2-r2)^2))
   power<-mean(as.numeric(data$p<.05))
   ptrunc<-mean(as.numeric(data$trunc))
   covered <- mean(as.numeric((data$cilo <= r2) & (r2 <= data$ciup)))
   n_ok<-nrow(data)
   n_fail<-length(attr(data,"fail"))
   miss_lo <- mean(data$ciup < r2)         
   miss_hi <- mean(data$cilo >  r2)       
   data.frame(er2=mer2,aer2=maer2,bias=bias,
              rmse=rmse,abias=abias,armse=armse,
              power=power,
              n_ok=n_ok,n_fail=n_fail,p_trunc=ptrunc,
              covered=covered,miss_lo=miss_lo,miss_hi=miss_hi)
   
   
}

runner<-Rsimcity::Runner$new("test")
runner$parallel=TRUE
runner$par_method="multicore"
runner$step<-one_model
runner$aggregate<-agg_fun

##### use this to test the sim ########### 
#runner$params<-list(rho=.3,r2=0,N=50,model="logistic",k=3)
#runner$test_one()
#runner$test_aggregates()
######    ----   #########

runner$params<-list(rho=.3)
runner$design<-list(r2=seq(.0,.3,.03),N=c(30,60,90,120),k=c(3,5),model=c("gaussian", "logistic","multinomial","ordinal"))

results<-runner$experiment(Rep=5000)
save(results,file=here::here("simulations/Data/study1.a.data.Rdata"))

