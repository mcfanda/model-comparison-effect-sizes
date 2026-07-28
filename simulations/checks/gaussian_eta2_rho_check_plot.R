# ------------------------------------------------------------------------
# Does the Gaussian eta^2/epsilon^2 bias pattern depend on how correlated
# the nuisance predictors are? Plots study1.b.check.data.Rdata (a dedicated
# check run: gaussian model only, rho = 0 vs .3 crossed with the usual
# N = {30,60,90,120}, k = {3,5}, eta2 = .03 to .30 by .03 grid, Rep = 1000).
#
# If the increasingly-negative adjustment bias at large eta2/small N is
# driven by collinearity among predictors (as the main check suggested),
# it should be much smaller or absent at rho = 0 (orthogonal predictors)
# than at rho = .3.
# ------------------------------------------------------------------------

library(ggplot2)
library(patchwork)

source(here::here("github/simulations/plot_helpers.R"))   # row_theme

load(here::here("github/simulations/checks/study1.b.check.data.Rdata"))   # -> `results`

results$raw_bias <- results$eeta2  - results$eta2
results$adj_bias <- results$aeeta2 - results$eta2
results$rho <- factor(results$rho, levels = c(0, .3), labels = c("rho = 0", "rho = .3"))
results$k   <- factor(results$k, levels = c(3, 5))

rho_colors <- c("rho = 0" = "#F8766D", "rho = .3" = "#00BFC4")

bias_panel <- function(data, y, y_lab, title) {
  ggplot(data, aes(x = eta2, y = .data[[y]], color = rho)) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
    geom_line(linewidth = 1) +
    facet_grid(k ~ N, labeller = labeller(N = label_both, k = label_both)) +
    scale_color_manual(values = rho_colors, name = NULL) +
    labs(x = expression(paste("Population ", eta^2)), y = y_lab, title = title) +
    row_theme
}

fig_rho <- bias_panel(results, "raw_bias", "Bias (raw eta2)", "Raw eta2") /
           bias_panel(results, "adj_bias", "Bias (adjusted eps2)", "Adjusted epsilon2") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_rho
ggsave(here::here("github/simulations/checks/gaussian_eta2_rho_check_plot.jpg"), fig_rho,
       width = 10, height = 9, dpi = 300)
