  library(ggplot2)
  library(patchwork)
  library(gzlmpower)

load(file = here::here("github/simulations/Data/study1.a.data.Rdata"))   # -> `results`
source(here::here("github/simulations/functions.R"))
source(here::here("github/simulations/plot_helpers.R"))   # to_long(), stack_by_model(), row_theme, model_labels

## computes the theoretical power

results$power_r2    <- mapply(theoretical_power, results$er2,   results$N, results$k, results$model)
results$power_adjr2 <- mapply(theoretical_power, results$aer2, results$N, results$k, results$model)
results$model_label <- factor(model_labels[results$model], levels = model_labels)

## ------------------------------------------------------------------------
## Figure 1 (accuracy): estimated R^2 and adjusted R^2 vs. population R^2,
## with the identity line marking perfect recovery.
## ------------------------------------------------------------------------


acc_long <- to_long(results, c(er2 = "R2", aer2 = "adj R2"),
                    id_cols = c("model_label", "N", "r2", "k"))
acc_long$Index <- factor(acc_long$Index, levels = c("R2", "adj R2"))
acc_long$k <- factor(acc_long$k, levels = c(3, 5))

fig1_colors    <- c("R2" = "#F8766D", "adj R2" = "#00BFC4")
fig1_linetypes <- c("3" = "solid", "5" = "dashed")

figure1 <- stack_by_model_k(acc_long, "r2", "value", fig1_colors, fig1_linetypes,
                            y_lab = "Index value", x_lab = expression(paste("Population ", R^2)),
                            ref_line = TRUE,
                            color_labels = expression(R^2, R[adj]^2))
figure1
ggsave(here::here("paper/Submission3/figure1.jpg"), figure1,
       width = 8, height = 10, dpi = 300)


## ------------------------------------------------------------------------
## Figure 3 (power): actual simulated power vs. the two closed-form power
## curves obtained from the mean raw/adjusted R^2 estimates.
## ------------------------------------------------------------------------

pow_long <- to_long(results, c(power = "Actual", power_r2 = "R2", power_adjr2 = "adj R2"),
                    id_cols = c("model_label", "N", "r2", "k"))
pow_long$Index <- factor(pow_long$Index, levels = c("Actual", "adj R2", "R2"))
pow_long$k <- factor(pow_long$k, levels = c(3, 5))

fig3_colors    <- c("Actual" = "black", "adj R2" = "#00BFC4", "R2" = "#F8766D")
fig3_linetypes <- c("3" = "solid", "5" = "dashed")

figure3 <- stack_by_model_k(pow_long, "r2", "value", fig3_colors, fig3_linetypes,
                            y_lab = expression(paste("Power (", 1 - beta, ")")),
                            x_lab = expression(paste("Population ", R^2)),
                            ref_line = FALSE,
                            color_legend = "Method",
                            color_labels = expression("Actual", R[adj]^2, R^2))
figure3
ggsave(here::here("paper/Submission3/figure3.jpg"), figure3,
       width = 8, height = 10, dpi = 300)
