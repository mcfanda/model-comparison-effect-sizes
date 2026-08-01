  library(ggplot2)
  library(patchwork)
  library(gzlmpower)

source(here::here("github/simulations/functions.R")) ## for model_labels
  
load(file = here::here("github/simulations/Data/study1.b.supp.data.Rdata"))   # -> `results`

## ------------------------------------------------------------------------
## These are the results of supplementary simulations run exactly like
## study1.b but with nuisance parameter (other predictors effect) set
## to 1/2 of the focal effect.
## Curves obtained from ETA-SQUARED and EPSILON-SQUARED estimates.
## ------------------------------------------------------------------------


### INDIVIDUAL  PREDICTOR ####

results$df_test <- sapply(results$model, focal_df)
## power.lrt()'s df-vs-categories sanity check is tuned for nominal (multinomial)
## coding and fires spuriously for the ordinal model, which by design uses 1 df
## per predictor regardless of the number of outcome categories -- expected, not
## an error, so it's suppressed here.
suppressMessages({
  results$power_eta2   <- mapply(theoretical_power, results$eeta2,  results$N, results$k, results$model, results$df_test)
  results$power_adjeta2 <- mapply(theoretical_power, results$aeeta2, results$N, results$k, results$model, results$df_test)
})

results$model_label <- factor(model_labels[results$model], levels = model_labels)

## ------------------------------------------------------------------------
## Figure1 (accuracy): estimated ETA-SQUARED and EPSILON-SQUARED vs. population ETA^2,
## with the identity line marking perfect recovery.
## ------------------------------------------------------------------------

figure<-Rsimcity::plot_by_splits(results,xvar="eta2",yvar=c("eeta2","aeeta2"),zvar="k",splits=c("N","model_label") ,
                                 title = c("Logistic","Multinomial","Ordinal","Gaussian"), titles = "top",
                                 ylabel="Index value",
                                 xlabel = expression(paste("Population ", eta^2)),
                                 color_labels = c(expression(eta^2),expression(epsilon^2)), color_label = "Index" 
)

figure <- figure + ggplot2::geom_abline(intercept = 0, slope = 1)


figure <- figure + ggplot2::theme(
  plot.title = ggplot2::element_text(size = 13),
  strip.text.x = ggplot2::element_text(size = 10)
)
figure <- figure + ggplot2::theme(
  panel.spacing.y = grid::unit(10, "pt")
)
figure
ggsave(here::here("github/supplementary/figure_sup_1.jpg"), figure,
       width = 8, height = 10, dpi = 300)


## ------------------------------------------------------------------------
## Figure 2 (power): actual simulated power vs. the two closed-form power
## curves obtained from ETA-SQUARED and EPSILON-SQUARED estimates.
## ------------------------------------------------------------------------

figure<-Rsimcity::plot_by_splits(results,xvar="eta2",yvar=c("power", "power_eta2","power_adjeta2"),zvar="k",splits=c("N","model_label") ,
                                 title = c("Logistic","Multinomial","Ordinal","Gaussian"), titles = "top",
                                 ylabel="Index value",
                                 xlabel = expression(paste("Population ", eta^2)),
                                 color_labels = expression("Actual", eta^2,epsilon^2), color_label = "Index" 
)

figure <- figure + ggplot2::scale_colour_manual(values = c("black","#F8766D", "#00BFC4"),labels =  expression("Actual", eta^2,epsilon^2))
figure <- figure + ggplot2::theme(
  plot.title = ggplot2::element_text(size = 13),
  strip.text.x = ggplot2::element_text(size = 10)
)
figure <- figure + ggplot2::theme(
  panel.spacing.y = grid::unit(10, "pt")
)
figure
ggsave(here::here("github/supplementary/figure_sup_2.jpg"), figure,
       width = 8, height = 10, dpi = 300)

