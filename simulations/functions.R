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
