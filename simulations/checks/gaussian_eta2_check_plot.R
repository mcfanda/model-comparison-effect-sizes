# ------------------------------------------------------------------------
# Plot comparing the main simulation pipeline's Gaussian eta^2 results
# (study1.b.data.Rdata, via Rsimcity::sample_by_eta2() + gzlmpower::eta2())
# against the fully independent, closed-form, non-quadrature, non-package
# check in gaussian_eta2_independent_check.R.
#
# If the two agree (within Monte Carlo noise), the increasingly-negative
# adjustment bias seen at large eta2/small N for the Gaussian model is a
# genuine property of the epsilon^2 correction under correlated predictors,
# not an artifact of the Rsimcity/gzlmpower pipeline.
# ------------------------------------------------------------------------

library(ggplot2)
library(patchwork)

source(here::here("github/simulations/plot_helpers.R"))   # row_theme

load(here::here("github/simulations/checks/gaussian_eta2_independent_check.Rdata"))   # -> `res`
indep <- res
indep$raw_bias <- indep$raw_mean - indep$eta2
indep$adj_bias <- indep$adj_mean - indep$eta2
indep$Source <- "Independent"

e1b <- new.env()
load(here::here("github/simulations/Data/study1.b.data.Rdata"), envir = e1b)
pipe <- e1b$results[e1b$results$model == "gaussian", ]
pipe$raw_bias <- pipe$eeta2 - pipe$eta2
pipe$adj_bias <- pipe$aeeta2 - pipe$eta2
pipe$Source <- "Pipeline"

cols <- c("eta2", "N", "k", "raw_bias", "adj_bias", "Source")
both <- rbind(indep[, cols], pipe[, cols])
both$k <- factor(both$k, levels = c(3, 5))
both$Source <- factor(both$Source, levels = c("Pipeline", "Independent"))

source_colors <- c("Pipeline" = "#F8766D", "Independent" = "#00BFC4")

bias_panel <- function(data, y, y_lab, title) {
  ggplot(data, aes(x = eta2, y = .data[[y]], color = Source, linetype = k)) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
    geom_line(linewidth = 1) +
    facet_wrap(~ N, nrow = 1, labeller = label_both) +
    scale_color_manual(values = source_colors, name = "Source") +
    scale_linetype_manual(values = c("3" = "solid", "5" = "dashed"), name = "k") +
    labs(x = expression(paste("Population ", eta^2)), y = y_lab, title = title) +
    row_theme
}

fig_check <- bias_panel(both, "raw_bias", "Bias (raw eta2)", "Raw eta2") /
             bias_panel(both, "adj_bias", "Bias (adjusted eps2)", "Adjusted epsilon2") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_check
ggsave(here::here("github/simulations/checks/gaussian_eta2_check_plot.jpg"), fig_check,
       width = 10, height = 7, dpi = 300)

## ------------------------------------------------------------------------
## Second plot: estimated eta2 and epsilon2 (not bias) vs. population eta2,
## with the identity line marking perfect recovery -- the same style as the
## main paper's Figure 2, reproduced separately for the pipeline (Rsimcity +
## gzlmpower) and the independent (closed-form, non-package) check, so the
## two can be compared directly on the original index scale.
## ------------------------------------------------------------------------

indep_val <- indep[, c("eta2", "N", "k")]
indep_val$raw <- indep$raw_mean
indep_val$adj <- indep$adj_mean
indep_val$Source <- "Independent"

pipe_val <- pipe[, c("eta2", "N", "k")]
pipe_val$raw <- pipe$eeta2
pipe_val$adj <- pipe$aeeta2
pipe_val$Source <- "Pipeline"

value_both <- rbind(indep_val, pipe_val)
value_long <- to_long(value_both, c(raw = "Eta2", adj = "Eps2"), id_cols = c("eta2", "N", "k", "Source"))
value_long$Index  <- factor(value_long$Index, levels = c("Eta2", "Eps2"))
value_long$k      <- factor(value_long$k, levels = c(3, 5))
value_long$Source <- factor(value_long$Source, levels = c("Pipeline", "Independent"))

value_colors <- c("Eta2" = "#F8766D", "Eps2" = "#00BFC4")

value_panel <- function(data, title) {
  ggplot(data, aes(x = eta2, y = value, color = Index, linetype = k)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.6, color = "black") +
    geom_line(linewidth = 1) +
    facet_wrap(~ N, nrow = 1, labeller = label_both) +
    scale_color_manual(values = value_colors, name = "Index", labels = expression(eta^2, epsilon^2)) +
    scale_linetype_manual(values = c("3" = "solid", "5" = "dashed"), name = "k") +
    labs(x = expression(paste("Population ", eta^2)), y = "Index value", title = title) +
    row_theme
}

fig_value <- value_panel(value_long[value_long$Source == "Pipeline", ],    "Pipeline") /
             value_panel(value_long[value_long$Source == "Independent", ], "Independent") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_value
ggsave(here::here("github/simulations/checks/gaussian_eta2_check_value_plot.jpg"), fig_value,
       width = 10, height = 7, dpi = 300)
