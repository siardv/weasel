# weasel

<!-- badges: start -->
[![R-CMD-check](https://github.com/siardv/weasel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siardv/weasel/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Wave-based Extraction and Selection for Longitudinal Data.

Tools for selecting, filtering, and balancing longitudinal panel data
across survey waves. A respondent counts as observed at a wave when a
row with that (id, wave) pair exists in the long-format data; a missed
wave is an absent row.

## Installation

```r
# install.packages("remotes")
remotes::install_github("siardv/weasel", build_vignettes = TRUE)
```

## Quick start

```r
library(weasel)

d <- generate_weasel_dummy_data(n_ids = 300, n_times = 12, seed = 1)

# scenario planning
p <- weasel_plan(d, id = "id", wave = "time", span = "core")
p  # compact print: span, scenario table, attached data size

cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenarios")

analysis_data <- weasel_apply(p, "anchored_balanced")
cat(weasel_justify_subset(p, "anchored_balanced"))

# audit the selection
weasel_print_table(weasel_sensitivity(p, max_missing = 0:2), n = 10)
weasel_print_table(weasel_selectivity(p, "anchored_balanced"))

# interactive pattern exploration
set_weasel_scope(d, "id", "time", gap = 1)
evaluate_weasel_scope()
weasel_reshape_to_wide()
weasel_summarize_waves()
weasel_print_table(weasel_filter_wave_summary(), n = 10)
weasel_scope_info()
weasel_clear_scope()
```

Panels with non-consecutive schedules (biennial waves, waves recorded
as years) are supported through `grid = "observed"` in `weasel_plan()`
and `set_weasel_scope()`.

See the [guide](https://siardv.github.io/weasel/) for a full walkthrough, or the
vignettes `vignette("introduction", package = "weasel")` and
`vignette("advanced-usage", package = "weasel")`.

## Two pipelines

| Pipeline | Entry point | Purpose |
|---|---|---|
| Scope | `set_weasel_scope()` | Interactive exploration of wave patterns |
| Plan  | `weasel_plan()` | Named, comparable, defensible selection scenarios |

Both share the same structural vocabulary: endpoints (observed first
and last wave of the window) and interior gaps (runs of missing waves
strictly between a respondent's first and last observed wave).

## Diagnostics

* `weasel_sensitivity()` sweeps the selection tolerances and reports
  the retained sample size for every combination.
* `weasel_selectivity()` compares retained and excluded respondents on
  covariates (standardized mean differences) to check whether
  completeness-based selection skews the sample.
* `weasel_scope_info()` prints the state of the active scope.

## Options

`options(weasel.verbose = FALSE)` silences all status messages.

## License

MIT
