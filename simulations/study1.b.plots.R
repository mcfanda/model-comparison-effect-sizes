  library(ggplot2)
  library(patchwork)
  library(gzlmpower)

load(file = "simulations/Data/study1.b.data.Rdata")   # -> `results`
source("simulations/functions.R")
source("simulations/plot_helpers.R")   # to_long(), stack_by_model(), row_theme, model_labels

## ------------------------------------------------------------------------
## Closed-form ("theoretical") power for each design cell, computed by
## plugging the cell's own MEAN raw estimate (eeta2) or MEAN adjusted estimate
## (aeeta2) into the power formula for the focal predictor's own test df
## (1, or yl-1=2 for multinomial -- see focal_df() in functions.R),
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

acc_long <- to_long(results, c(eeta2 = "Eta2", aeeta2 = "adj Eta2"),
                     id_cols = c("model_label", "N", "eta2"))
acc_long$Index <- factor(acc_long$Index, levels = c("Eta2", "adj Eta2"))

fig2_colors <- c("Eta2" = "#F8766D", "adj Eta2" = "#00BFC4")

figure2 <- stack_by_model(acc_long, "eta2", "value", fig2_colors, "Index",
                           y_lab = "Index value", x_lab = expression(paste("Population ", eta^2)),
                           ref_line = TRUE,
                           legend_labels = expression(eta^2, epsilon^2))
figure2
ggsave("paper/Submission3/figure2.jpg", figure2,
       width = 8, height = 8, dpi = 300)

## ------------------------------------------------------------------------
## Figure 4 (power): actual simulated power (for the focal predictor's own
## test) vs. the two closed-form power curves obtained from the mean
## raw/adjusted eta^2 estimates.
## ------------------------------------------------------------------------

pow_long <- to_long(results, c(power = "Actual", power_eta2 = "Eta2", power_adjeta2 = "adj Eta2"),
                     id_cols = c("model_label", "N", "eta2"))
pow_long$Index <- factor(pow_long$Index, levels = c("Actual", "adj Eta2", "Eta2"))

fig4_colors <- c("Actual" = "black", "adj Eta2" = "#00BFC4", "Eta2" = "#F8766D")

figure4 <- stack_by_model(pow_long, "eta2", "value", fig4_colors, "Method",
                           y_lab = expression(paste("Power (", 1 - beta, ")")),
                           x_lab = expression(paste("Population ", eta^2)),
                           ref_line = FALSE,
                           legend_labels = expression("Actual", epsilon^2, eta^2))
figure4
ggsave("paper/Submission3/figure4.jpg", figure4,
       width = 8, height = 8, dpi = 300)

