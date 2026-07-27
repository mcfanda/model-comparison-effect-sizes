suppressMessages(library(gzlmpower))

load(file = "simulations/Data/study1.a.data.Rdata")   # -> `results`
source("simulations/functions.R")

model_labels <- c(gaussian = "Gaussian", logistic = "Logistic",
                   multinomial = "Multinomial", ordinal = "Ordinal")
results$model_label <- factor(model_labels[results$model], levels = model_labels)

## ------------------------------------------------------------------------
## 1. Accuracy: mean absolute bias and RMSE for the raw and adjusted index,
## collapsed across the 16 population-R^2 values within each Model x N cell.
## ------------------------------------------------------------------------

acc_summary <- aggregate(
  cbind(abs_bias_raw = abs(bias), rmse_raw = rmse,
        abs_bias_adj = abs(abias), rmse_adj = armse) ~ model_label + N,
  data = results, FUN = mean
)
acc_summary <- acc_summary[order(acc_summary$model_label, acc_summary$N), ]
rownames(acc_summary) <- NULL

## ------------------------------------------------------------------------
## 2. Power agreement: mean absolute difference between the actual
## (simulated) power and the closed-form power obtained from the cell's mean
## adjusted R^2 estimate -- the reviewer's "litmus test" comparison.
## ------------------------------------------------------------------------

results$power_adjr2 <- mapply(theoretical_power, results$aer2, results$N, results$k, results$model)
results$power_gap   <- abs(results$power_adjr2 - results$power)

power_summary <- aggregate(power_gap ~ model_label + N, data = results, FUN = mean)
power_summary <- power_summary[order(power_summary$model_label, power_summary$N), ]
rownames(power_summary) <- NULL

power_gap_overall  <- mean(results$power_gap)
power_gap_worst_row <- results[which.max(results$power_gap),
                                c("model_label", "N", "r2", "power", "power_adjr2", "power_gap")]

## ------------------------------------------------------------------------
## 3. Convergence: percentage of replications excluded (non-convergence or
## quasi-complete separation) per Model x N cell, collapsed across R^2.
## ------------------------------------------------------------------------

conv_totals <- aggregate(cbind(n_ok, n_fail) ~ model_label + N, data = results, FUN = sum)
conv_totals$pct_excluded <- 100 * conv_totals$n_fail / (conv_totals$n_ok + conv_totals$n_fail)
conv_summary <- conv_totals[order(conv_totals$model_label, conv_totals$N),
                             c("model_label", "N", "pct_excluded")]
rownames(conv_summary) <- NULL

## the flagged cell (multinomial, N=25) reported by R^2 range, for the footnote
mult25 <- results[results$model == "multinomial" & results$N == 25, ]
mult25 <- mult25[order(mult25$r2), ]
mult25_pct <- 100 * mult25$n_fail / (mult25$n_ok + mult25$n_fail)
mult25_excl_range <- range(mult25_pct)
mult25_excl_mean  <- mean(mult25_pct)

## ------------------------------------------------------------------------
## Save everything the manuscript needs, plus the full per-cell table for
## supplementary material.
## ------------------------------------------------------------------------

save(acc_summary, power_summary, power_gap_overall, power_gap_worst_row,
     conv_summary, mult25_excl_range, mult25_excl_mean, results,
     file = "simulations/Data/study1.a.summary.Rdata")

write.csv(results, "simulations/Data/study1.a.full_results.csv", row.names = FALSE)

cat("Accuracy summary (mean |bias| and RMSE, collapsed over R^2):\n")
print(acc_summary, digits = 3)

cat("\nPower agreement (mean |actual - closed-form(adj R2)|, collapsed over R^2):\n")
print(power_summary, digits = 3)
cat("overall mean gap:", round(power_gap_overall, 3), "\n")

cat("\nExclusion rate (%), collapsed over R^2:\n")
print(conv_summary, digits = 3)
cat("multinomial, N=25, exclusion rate across R^2: ",
    round(mult25_excl_range[1]), "% to ", round(mult25_excl_range[2]),
    "% (mean ", round(mult25_excl_mean), "%)\n", sep = "")

