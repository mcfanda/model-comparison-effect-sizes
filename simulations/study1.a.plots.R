  library(ggplot2)
  library(patchwork)
  library(gzlmpower)

load(file = "simulations/Data/study1.a.data.Rdata")   # -> `results`
source("simulations/functions.R")
source("simulations/plot_helpers.R")   # to_long(), stack_by_model(), row_theme, model_labels

## computes the theoretical power

results$power_r2    <- mapply(theoretical_power, results$er2,   results$N, results$k, results$model)
results$power_adjr2 <- mapply(theoretical_power, results$aer2, results$N, results$k, results$model)
results$model_label <- factor(model_labels[results$model], levels = model_labels)

## ------------------------------------------------------------------------
## Figure 1 (accuracy): estimated R^2 and adjusted R^2 vs. population R^2,
## with the identity line marking perfect recovery.
## ------------------------------------------------------------------------

acc_long <- to_long(results, c(er2 = "R2", aer2 = "adj R2"),
                     id_cols = c("model_label", "N", "r2"))
acc_long$Index <- factor(acc_long$Index, levels = c("R2", "adj R2"))

fig1_colors <- c("R2" = "#F8766D", "adj R2" = "#00BFC4")

figure1 <- stack_by_model(acc_long, "r2", "value", fig1_colors, "Index",
                           y_lab = "Index value", x_lab = expression(paste("Population ", R^2)),
                           ref_line = TRUE,
                           legend_labels = expression(R^2, R[adj]^2))
figure1
ggsave("paper/Submission3/figure1.jpg", figure1,
       width = 8, height = 10, dpi = 300)

## ------------------------------------------------------------------------
## Figure 3 (power): actual simulated power vs. the two closed-form power
## curves obtained from the mean raw/adjusted R^2 estimates.
## ------------------------------------------------------------------------

pow_long <- to_long(results, c(power = "Actual", power_r2 = "R2", power_adjr2 = "adj R2"),
                     id_cols = c("model_label", "N", "r2"))
pow_long$Index <- factor(pow_long$Index, levels = c("Actual", "adj R2", "R2"))

fig3_colors <- c("Actual" = "black", "adj R2" = "#00BFC4", "R2" = "#F8766D")

figure3 <- stack_by_model(pow_long, "r2", "value", fig3_colors, "Method",
                           y_lab = expression(paste("Power (", 1 - beta, ")")),
                           x_lab = expression(paste("Population ", R^2)),
                           ref_line = FALSE,
                           legend_labels = expression("Actual", R[adj]^2, R^2))
figure3
ggsave("paper/Submission3/figure3.jpg", figure3,
       width = 8, height = 10, dpi = 300)
