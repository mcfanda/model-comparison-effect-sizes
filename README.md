# Model-comparison effect size indices for GzLMs

Code, simulations, and supplementary material for:

> Gallucci, M. *Model-comparison effect size indices for logistic and other
> generalized linear models: Computation and power analysis.* Department of
> Psychology, University of Milano-Bicocca.

The paper discusses model-comparison effect size indices (pseudo-R², η², and
their bias-adjusted ε² counterparts) for generalized linear models —
gaussian, logistic, multinomial, and ordinal (proportional-odds) — including
partial versions for individual predictors, and their use in power analysis.

## Repository structure

- **`simulations/`** — the two simulation studies reported in the paper.
  - `study1.a.R` / `study1.b.R` — Study 1: recovery of the whole-model R²
    (`.a`) and of a single focal predictor's η² (`.b`), crossing 4 models
    (gaussian, logistic, multinomial, ordinal) × 10 population effect sizes
    × 4 sample sizes × 2 numbers of covariates (k = 3 or 5).
  - `study1.a.summary.R`, `study1.a.plots.R`, `study1.b.plots.R` — summary
    statistics and figures (accuracy, power) built from Study 1's results,
    with k = 3 (solid line) vs. k = 5 (dashed line) shown separately in every
    panel.
  - `study2.a.R` / `study2.b.R` — Study 2: simulation-based required sample
    size for a target power, for the omnibus test (`.a`) and the
    single-predictor test (`.b`), compared against the closed-form estimate.
  - `study2.plots.R` — Figure comparing simulated vs. closed-form required N.
  - `functions.R`, `plot_helpers.R` — shared helper functions used across the
    scripts above, including the closed-form power calculations
    (`theoretical_power()`) used as the benchmark against which the
    simulations are compared.
  - `Data/` — saved simulation results (`.Rdata`/`.csv`).
  - `checks/` — additional, standalone verification scripts (not simulation
    studies reported as such in the paper) probing an unusual finding in
    Study 1b: at small N, the adjusted ε² for a single focal predictor shows
    a bias that grows with the true population η² and with the number of
    covariates k. These scripts show that the pattern (a) reproduces almost
    exactly in a fully independent, closed-form implementation that uses
    neither the `Rsimcity` nor the `gzlmpower` package, (b) does not depend
    on the correlation between the focal predictor and the nuisance
    covariates (ρ = 0 vs. ρ = .3 give the same result), and is therefore a
    genuine finite-sample property of the correction (driven by the
    reduced-model refit needed to isolate a single predictor's unique
    contribution), not a bug or an artifact of collinearity. This issue does
    not affect the omnibus R² and is out of scope for the present paper
    (Gaussian is included there only as a GLM benchmark), but is noted here
    as a starting point for anyone wanting to follow up on it.
- **`supplementary/`** — self-contained supplementary material.
  - `SM_code.Rmd`/`.pdf` — a from-scratch reproduction of the paper's
    Gauss-Hermite quadrature calibration method for the logistic,
    multinomial, and ordinal models, independent of the `Rsimcity` package,
    so it can be followed and run without installing it.
  - `SM_table.Rmd`/`.pdf` — full results tables (accuracy and power, by
    model, sample size, and number of covariates k) for the data plotted in
    the paper's Figures 1-4, plus the full underlying data for Study 2
    (Figure 5).

## Dependencies

The simulations rely on two companion R packages developed for this paper:

- [`Rsimcity`](https://github.com/mcfanda/Rsimcity) — generates samples with
  an exactly known (quadrature-calibrated) population R²/η², and provides the
  `Runner` framework used to run the simulation designs.
- [`gzlmpower`](https://github.com/mcfanda/gzlmpower) — computes the effect
  size indices (R², η², ε², adjusted R²) and the power/required-N functions
  discussed in the paper.

Both are installed automatically by the simulation scripts via
`remotes::install_github()` if not already present.
