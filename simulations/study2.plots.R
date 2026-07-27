library(ggplot2)
library(patchwork)

## study2.a.data.Rdata and study2.b.data.Rdata both save an object literally
## named `results` -- load each into its own environment to avoid clobbering.
e <- new.env(); load(file = "simulations/Data/study2.a.data.Rdata", envir = e); omni  <- e$results
e <- new.env(); load(file = "simulations/Data/study2.b.data.Rdata", envir = e); indiv <- e$results

source("simulations/plot_helpers.R")   # to_long(), row_theme, model_labels

## No gaussian benchmark in Study 2 (see manuscript's "Required sample size" section).
model_levels <- c("logistic", "multinomial", "ordinal")
model_lab_levels <- model_labels[model_levels]

## ------------------------------------------------------------------------
## Reshape each study's design cells (one row per model x population effect
## size) into long form: `Actual` is the simulated minimal required N (find_n()'s
## search result), `Estimated` is the closed-form N obtained by inverting the
## power formula at the population effect size itself (Nt, the search's own
## starting guess).
## ------------------------------------------------------------------------

prep <- function(data, xvar) {
  data$model_label <- factor(model_labels[data$model], levels = model_lab_levels)
  long <- to_long(data, c(N = "Actual", Nt = "Estimated"), id_cols = c("model_label", xvar))
  long$Index <- factor(long$Index, levels = c("Actual", "Estimated"))
  long
}

omni_long  <- prep(omni,  "r2")
indiv_long <- prep(indiv, "eta2")

req_n_colors <- c("Actual" = "black", "Estimated" = "#00BFC4")

panel <- function(data, xvar, x_lab, show_x, title) {
  ggplot(data, aes(x = .data[[xvar]], y = value, color = Index)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = req_n_colors, name = NULL) +
    labs(x = if (show_x) x_lab else NULL, y = "Required N", title = title) +
    row_theme
}

r2_lab   <- expression(paste("Population ", R^2))
eta2_lab <- expression(paste("Population ", eta^2))

panels <- list()
for (i in seq_along(model_levels)) {
  lab <- model_lab_levels[[model_levels[i]]]
  is_last <- i == length(model_levels)
  panels[[length(panels) + 1]] <- panel(omni_long[omni_long$model_label == lab, ], "r2",
                                         r2_lab, is_last, paste0(lab, " (omnibus test)"))
}
for (i in seq_along(model_levels)) {
  lab <- model_lab_levels[[model_levels[i]]]
  is_last <- i == length(model_levels)
  panels[[length(panels) + 1]] <- panel(indiv_long[indiv_long$model_label == lab, ], "eta2",
                                         eta2_lab, is_last, paste0(lab, " (individual predictor)"))
}

figure5 <- wrap_plots(panels, ncol = 2, byrow = FALSE) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
figure5

ggsave("paper/Submission3/figure5.jpg", figure5, width = 10, height = 9, dpi = 300)
