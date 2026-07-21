# weasel

<!-- badges: start -->
[![R-CMD-check](https://github.com/siardv/weasel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siardv/weasel/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**`weasel` turns wave-and-respondent selection in longitudinal panels into an explicit, reproducible workflow.**

**Workflow guide:** <https://siardv.github.io/weasel/>

Longitudinal analyses rarely use every respondent at every wave, and the
selection step (which waves, which respondents, how much missingness to
tolerate) usually ends up as a few undocumented lines of filtering code.
`weasel` (Wave-based Extraction and Selection for Longitudinal Data)
makes that step explicit:

- **Explore.** `set_weasel_scope()` gives interactive access to wave
  patterns, gaps, and completeness.
- **Plan.** `weasel_plan()` turns selection rules into named,
  comparable scenarios.
- **Audit.** `weasel_sensitivity()` and `weasel_selectivity()` show
  what each scenario costs and whom it excludes.
- **Justify.** `weasel_apply()` extracts the analysis sample;
  `weasel_justify_subset()` drafts the written rationale.

One structural convention drives everything: a respondent is observed
at a wave when a row with that `(id, wave)` pair exists in the
long-format data; a missed wave is an absent row.

> **Repository history note:** Some early, pre-`v0.3.1` development
> history was consolidated while the repository workflow was being refined.
> A forthcoming update will add brief archival provenance information;
> the current releases and documentation remain canonical.

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
p  # compact print: span, population, scenario table, attached data size

# a design-fixed window instead of the automatic core rule
p_fixed <- weasel_plan(d, id = "id", wave = "time", lower = 3, upper = 8)

cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenarios")

analysis_data <- weasel_apply(p, "anchored_balanced")
cat(weasel_justify_subset(p, "anchored_balanced"))

# audit the selection
weasel_print_table(weasel_sensitivity(p, max_missing = 0:2), n = 10)
weasel_print_table(weasel_selectivity(p, "anchored_balanced"))

# interactive pattern exploration
set_weasel_scope(d, "id", "time", max_gap_len = 1)
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
* Plans record their planning population (the denominator behind every
  retention figure) and a structural fingerprint of their data,
  including an order-invariant digest of the deduplicated (id, wave)
  assignments; reuniting a saved plan with explicitly re-supplied data
  that differ, even when every aggregate count coincides, warns
  (`weasel_data_mismatch`).

## Options

`options(weasel.verbose = FALSE)` silences all status messages.

## A note on how this package was built

I started building `weasel` in June 2021, before AI coding assistants were a
realistic option, and it has been a constant companion project ever
since. The problem it addresses, the selection logic, the architecture,
and the design decisions grew out of five years of building, discarding,
and rebuilding.

I also want to be open about the fact that AI language models
(including Anthropic's Claude) contributed to later versions. I used
them as assistants, not as authors: to review code, stress-test logic,
propose refactorings, draft tests and documentation, and speed up the
grueling parts of package development. Nothing was accepted on trust.
Every suggestion was read, questioned, run, and frequently rejected or
rewritten; whatever ships has passed the full test suite and R CMD
check, and responsibility for every line, including the mistakes, is
mine alone.

`weasel` exists to make selection decisions in panel data explicit
instead of silent. It seems only consistent to be equally explicit
about how the package itself was made. If you have questions about any
part of that process, the issue tracker is open.

## License

MIT
