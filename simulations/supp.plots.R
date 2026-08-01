  library(ggplot2)
  library(patchwork)
  library(gzlmpower)

load(file = here::here("github/simulations/Data/study1.b.supp.data.Rdata"))   # -> `results`
source(here::here("github/simulations/plot_helpers.R"))   
## ------------------------------------------------------------------------
## Closed-form ("theoretical") power for each design cell, computed by
## plugging the cell's own MEAN raw estimate (eeta2) or MEAN adjusted estimate
## (aeeta2) into the power formula for the focal predictor's own test df
## (1, or yl-1=2 for multinomial -- see focal_df() in theoretical_power.R),
## not the whole model's df as in Study 1a. Compared against the "Actual"
## column already in `results` (the empirical proportion of significant
## per-predictor LR tests).
## ------------------------------------------------------------------------

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
## Figure 2 (accuracy): estimated eta^2 and adjusted epsilon^2 (aeeta2) vs.
## population eta^2, with the identity line marking perfect recovery.
## ------------------------------------------------------------------------

figure<-Rsimcity::plot_by_splits(acc_long,xvar="eta2",yvar="value",zvar="Index",splits=c("N","model_label") ,
                         title = c("Logistic","Multinomial","Ordinal","Gaussian"), titles = "top",
                         ylabel="Index value",
                         xlabel = expression(paste("Population ", eta^2)))

figure <- figure + ggplot2::geom_abline(intercept = 0, slope = 1)
figure <- figure +ggplot2::theme( plot.title = ggplot2::element_text(size = 11))
figure <- figure + ggplot2::theme(
  plot.title = ggplot2::element_text(size = 13),
  strip.text.x = ggplot2::element_text(size = 10)
)
figure <- figure + ggplot2::theme(
  panel.spacing.y = grid::unit(10, "pt")
)

figure
ggplot2::ggsave(here::here("github/supplementary/figure_sup_1.jpg"), figure,
       width = 8, height = 10, dpi = 300)


## ------------------------------------------------------------------------
## Figure 4 (power): actual simulated power (for the focal predictor's own
## test) vs. the two closed-form power curves obtained from the mean
## raw/adjusted eta^2 estimates.
## ------------------------------------------------------------------------

pow_long <- to_long(results, c(power = "Actual", power_eta2 = "Eta2", power_adjeta2 = "adj Eta2"),
                     id_cols = c("model_label", "N", "eta2", "k"))
pow_long$Index <- factor(pow_long$Index, levels = c("Actual", "adj Eta2", "Eta2"))
pow_long$k <- factor(pow_long$k, levels = c(3, 5))

fig4_colors    <- c("Actual" = "black", "adj Eta2" = "#00BFC4", "Eta2" = "#F8766D")
fig4_linetypes <- c("3" = "solid", "5" = "dashed")

figure4 <- stack_by_model_k(pow_long, "eta2", "value", fig4_colors, fig4_linetypes,
                             y_lab = expression(paste("Power (", 1 - beta, ")")),
                             x_lab = expression(paste("Population ", eta^2)),
                             ref_line = FALSE,
                             color_legend = "Method",
                             color_labels =
figure4
ggsave(here::here("paper/Submission3/figure4.jpg"), figure4,
       width = 8, height = 10, dpi = 300)

