## Shared plotting machinery for study1.a.plots.R and study1.b.plots.R: one row
## of N-facets per model, stacked with patchwork, colors/legend matching the
## style used throughout the manuscript's Figures 1-5 (salmon = raw index,
## teal = adjusted index, black = actual/reference).

library(ggplot2)
library(patchwork)

source("github/simulations/functions.R")   # model_labels

## ------------------------------------------------------------------------
## Reshape a wide data frame into long form 
## ------------------------------------------------------------------------

to_long <- function(data, value_cols, id_cols, names_to = "Index", values_to = "value") {
  do.call(rbind, lapply(names(value_cols), function(col) {
    out <- data[, id_cols, drop = FALSE]
    out[[values_to]] <- data[[col]]
    out[[names_to]]  <- value_cols[[col]]
    out
  }))
}

row_theme <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

stack_by_model <- function(data, xvar, y, colors, legend_name, y_lab, x_lab,
                           ref_line = FALSE, legend_labels = waiver()) {
  model_levels <- levels(data$model_label)
  panels <- lapply(model_levels, function(m) {
    is_last <- m == model_levels[length(model_levels)]
    d <- data[data$model_label == m, ]
    p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[y]], color = Index)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ N, nrow = 1, labeller = label_both) +
      scale_color_manual(values = colors, name = legend_name, labels = legend_labels) +
      labs(x = if (is_last) x_lab else NULL, y = y_lab, title = m) +
      row_theme
    if (ref_line) p <- p + geom_abline(slope = 1, intercept = 0, linewidth = 0.6, color = "black")
    p
  })
  wrap_plots(panels, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}


## ------------------------------------------------------------------------
## stack_by_model_k(): like stack_by_model() (plot_helpers.R), but for panels
## that show both indices (color) *and* both k conditions (linetype) at once,
## rather than a single line per Index. Kept local to this script since it's
## specific to study1.c's k=3-vs-k=5 comparison and study1.a/b's shared helper
## is left untouched.
## ------------------------------------------------------------------------

stack_by_model_k <- function(data, xvar, y, colors, linetypes, y_lab, x_lab,
                             ref_line = FALSE, color_legend = "Index", linetype_legend = "k",
                             color_labels = waiver(), linetype_labels = waiver()) {
  model_levels <- levels(data$model_label)
  panels <- lapply(model_levels, function(m) {
    is_last <- m == model_levels[length(model_levels)]
    d <- data[data$model_label == m, ]
    p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[y]], color = Index, linetype = k)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ N, nrow = 1, labeller = label_both) +
      scale_color_manual(values = colors, name = color_legend, labels = color_labels) +
      scale_linetype_manual(values = linetypes, name = linetype_legend, labels = linetype_labels) +
      labs(x = if (is_last) x_lab else NULL, y = y_lab, title = m) +
      row_theme
    if (ref_line) p <- p + geom_abline(slope = 1, intercept = 0, linewidth = 0.6, color = "black")
    p
  })
  wrap_plots(panels, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}
