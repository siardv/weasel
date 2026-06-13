# weasel

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
p   <- weasel_plan(d, id = "id", wave = "time", span = "core")
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenarios")

analysis_data <- weasel_apply(p, "anchored_balanced")
cat(weasel_justify_subset(p, "anchored_balanced"))

# interactive pattern exploration
set_weasel_scope(d, "id", "time", gap = 1)
evaluate_weasel_scope()
reshape_to_wide()
summarize_waves()
weasel_print_table(filter_wave_summary(), n = 10)
weasel_clear_scope()
```

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

## Options

`options(weasel.verbose = FALSE)` silences all status messages.

## License

MIT
