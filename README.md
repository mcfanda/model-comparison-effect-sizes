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
    (`.a`) and of a single focal predictor's η² (`.b`) across sample size and
    population effect size.
  - `study1.a.summary.R`, `study1.a.plots.R`, `study1.b.plots.R` — summary
    statistics and figures (accuracy, power) built from Study 1's results.
  - `study2.a.R` / `study2.b.R` — Study 2: simulation-based required sample
    size for a target power, for the omnibus test (`.a`) and the
    single-predictor test (`.b`), compared against the closed-form estimate.
  - `study2.plots.R` — Figure comparing simulated vs. closed-form required N.
  - `functions.R`, `plot_helpers.R` — shared helper functions used across the
    scripts above, including the closed-form power calculations
    (`theoretical_power()`) used as the benchmark against which the
    simulations are compared.
  - `Data/` — saved simulation results (`.Rdata`/`.csv`).
  - `Figures/` — supporting figures not reproduced from `Data/` above.
- **`supplementary/`** — self-contained supplementary material
  (`supplementary_material.Rmd`): a from-scratch reproduction of the paper's
  Gauss-Hermite quadrature calibration method for the logistic model,
  independent of the `Rsimcity` package, so it can be followed and run
  without installing it; plus full results tables for the data plotted in
  the paper's figures.

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
