####### paper plots

library(ggplot2)
library(patchwork)
library(gzlmpower)

#### STUDY 1 ####

### OMNIBUS R-SQUARED ####
source(here::here("github/simulations/functions.R")) ## for model_labels

load(file = here::here("github/simulations/Data/study1.a.data.Rdata"))   # -> `results`

## computes the theoretical power

results$power_r2    <- mapply(theoretical_power, results$er2,   results$N, results$k, results$model)
results$power_adjr2 <- mapply(theoretical_power, results$aer2, results$N, results$k, results$model)
results$model_label <- factor(model_labels[results$model], levels = model_labels)

## ------------------------------------------------------------------------
## Figure 1 (accuracy): estimated R^2 and adjusted R^2 vs. population R^2,
## with the identity line marking perfect recovery.
## ------------------------------------------------------------------------

figure<-Rsimcity::plot_by_splits(results,xvar="r2",yvar=c("er2","aer2"),zvar="k",splits=c("N","model_label") ,
                                 title = c("Logistic","Multinomial","Ordinal","Gaussian"), titles = "top",
                                 ylabel="Index value",
                                 xlabel = expression(paste("Population ", R^2)),
                                 color_label = "Index" 
                                 )

figure <- figure + ggplot2::geom_abline(intercept = 0, slope = 1)

figure <- figure + ggplot2::scale_colour_manual(values = c("#F8766D", "#00BFC4"),labels =  expression("Actual", R^2,R[adj]^2))
                                                
figure <- figure + ggplot2::theme(
  plot.title = ggplot2::element_text(size = 13),
  strip.text.x = ggplot2::element_text(size = 10)
)
figure <- figure + ggplot2::theme(
  panel.spacing.y = grid::unit(10, "pt")
)
figure
ggsave(here::here("paper/Submission3/figure1.jpg"), figure,
       width = 8, height = 10, dpi = 300)


## ------------------------------------------------------------------------
## Figure 3 (power): actual simulated power vs. the two closed-form power
## curves obtained from the mean raw/adjusted R^2 estimates.
## ------------------------------------------------------------------------


figure<-Rsimcity::plot_by_splits(results,xvar="r2",yvar=c("power","power_r2","power_adjr2"),zvar="k",splits=c("N","model_label") ,
                                 title = c("Logistic","Multinomial","Ordinal","Gaussian"), titles = "top",
                                 ylabel="Power",
                                 xlabel = expression(paste("Population ", R^2)),
                                 color_label = "Index" 
)

figure <- figure + ggplot2::scale_colour_manual(values = c("black" ,"#F8766D", "#00BFC4"),labels =  expression("Actual", R^2,R[adj]^2))
figure <- figure + ggplot2::theme(
  plot.title = ggplot2::element_text(size = 13),
  strip.text.x = ggplot2::element_text(size = 10)
)

figure
ggsave(here::here("paper/Submission3/figure3.jpg"), figure,
            width = 8, height = 10, dpi = 300)


### INDIVIDUAL  PREDICTOR ####

load(file = here::here("github/simulations/Data/study1.b.data.Rdata"))   # -> `results`
source(here::here("github/simulations/plot_helpers.R"))   

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
## Figure 2 (accuracy): estimated ETA-SQUARED and EPSILON-SQUARED vs. population ETA^2,
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
ggsave(here::here("paper/Submission3/figure2.jpg"), figure,
            width = 8, height = 10, dpi = 300)


## ------------------------------------------------------------------------
## Figure 4 (power): actual simulated power vs. the two closed-form power
## curves obtained from ETA-SQUARED and EPSILON-SQUARED estimates.
## ------------------------------------------------------------------------

figure<-Rsimcity::plot_by_splits(results,xvar="eta2",yvar=c("power", "power_eta2","power_adjeta2"),zvar="k",splits=c("N","model_label") ,
                                 title = c("Logistic","Multinomial","Ordinal","Gaussian"), titles = "top",
                                 ylabel="Power",
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
ggsave(here::here("paper/Submission3/figure4.jpg"), figure,
            width = 8, height = 10, dpi = 300)

####  STUDY 2 ##########

## ------------------------------------------------------------------------
## Figure 5 (required N):required N from R^2 and eta^2.
## ------------------------------------------------------------------------

### data for R-squared
load(file = here::here("github/simulations/Data/study2.a.data.Rdata")) # `results`
     
model_levels <- c("logistic", "multinomial", "ordinal")

figures1<-lapply(model_levels,function(model_level) {
  
  data<-results[results$model==model_level,]
  figure<-Rsimcity::plot_by_splits(data,xvar="r2",yvar=c("N", "Nt"),
                                   xlabel = expression(paste("Population ", R^2)),
                                   ylabel = "Required N",color_label = ""
                                   
  )
  figure<- figure + ggtitle(paste(model_labels[model_level],"(omnibus test)"))
  figure <- figure + ggplot2::scale_colour_manual(values = c("black", "#00BFC4"),labels =  expression("Actual", "Estimated"))
  figure <- figure + ggplot2::theme(
    plot.title = ggplot2::element_text(size = 13),
    strip.text.x = ggplot2::element_text(size = 10)
  )
  figure
  }
  )

### data for epsilon-squared

load(file = here::here("github/simulations/Data/study2.b.data.Rdata")) # `results`

figures2<-lapply(model_levels,function(model_level) {
  
  data<-results[results$model==model_level,]
  figure<-Rsimcity::plot_by_splits(data,xvar="eta2",yvar=c("N", "Nt"),
                                   xlabel = expression(paste("Population ", eta^2)),
                                   ylabel = "Required N", color_label = ""
  )
  figure<- figure + ggtitle(paste(model_labels[model_level],"(individual predictor test)"))
  figure <- figure + ggplot2::scale_colour_manual(values = c("black", "#00BFC4"),labels =  expression("Actual", "Estimated"))
  figure <- figure + ggplot2::theme(
    plot.title = ggplot2::element_text(size = 13),
    strip.text.x = ggplot2::element_text(size = 10)
  )
  figure
}
)
final_plot <- wrap_plots(
  as.vector(rbind(figures1, figures2)),
  ncol = 2
)

final_plot <- final_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(here::here("paper/Submission3/figure5.jpg"), final_plot,
       width = 8, height = 10, dpi = 300)



