# weasel 0.4.0

A consolidation release: corrections, honesty on imperfect input, one
constraint vocabulary, and transparency for every automatic decision.
No new modelling features. Results on clean panels with default
arguments are unchanged except where noted below; every behavioural
change is guarded by the expanded test suite (240 to 480 assertions).

## Breaking and behaviour changes

* One constraint vocabulary across both pipelines: `min_present`,
  `max_missing`, `n_gap_max`, `max_gap_len`, `require_endpoints`. In
  `set_weasel_scope()`, `size` (its minimum), `gap`, and `n_gap` are
  deprecated aliases of `min_present`, `max_gap_len`, and `n_gap_max`;
  scenario tables use `max_gap_len` in place of `max_gap_max`, and
  `weasel_sensitivity()` renames its argument and output column
  accordingly. All old names keep working through the 0.x series with
  classed warnings (`weasel_deprecated`); an explicit new-name argument
  always wins over its alias, and plans saved by older versions remain
  readable.
* The scope's default minimum presence is now `min_present = 1`:
  exploration shows every respondent with at least one observed wave
  instead of silently dropping those with fewer than three. Set
  `min_present = 3` to reproduce the old default.
* Scenario matching accepts an exact name or an unambiguous prefix;
  arbitrary substrings (for example `"strict"` for `anchored_strict`)
  no longer match.
* Strict integer validation everywhere a whole number is expected
  (`gap`/`max_gap_len`, `n_gap`/`n_gap_max`, `min_present`, `lower`,
  `upper`, `core_len`, sensitivity tolerances, scenario columns with
  `Inf` still meaning "no constraint", `digits`/`n` in
  `weasel_print_table()`, generator counts and `waves`). Fractional
  values are rejected instead of silently truncated (previously
  `gap = 1.9` acted as `1`) or rounded (previously `lower = 2.6` acted
  as `3`).
* `id` and `wave` must name different columns; rows dropped from
  participation analysis (missing id, missing wave, outside the span)
  are counted in a verbose-mode message in both pipelines.
* `weasel_justify_subset()` refuses scenarios that retain no
  respondents, and `weasel_compare_to_sentence()` states plainly when
  no scenario is recommendable instead of naming one (both classed;
  `weasel_error_empty_scenario` where applicable).

## Fixes

* The backwards-compatibility fallback for plan objects saved by
  pre-0.3 versions had never worked: with `$span` absent, `$` partial
  matching picked up `span_reason`, its `"full"`/`"core"` label
  coerced to `NA`, and `weasel_apply()`, `weasel_summarize_subset()`,
  and `weasel_selectivity()` silently returned zero rows. Exact
  `[["span"]]` indexing fixes it, with an end-to-end regression test.
* `weasel_selectivity()` was row-order dependent under duplicated
  (id, wave) rows with `at = "first"` and double-counted duplicated
  rows with `at = "mean"`. Covariate values are now averaged within
  each duplicated pair, a classed warning (`weasel_duplicates`) is
  emitted, and the diagnostic is order-invariant.
* `weasel_apply()` and `weasel_summarize_subset()` warn when the
  returned data still contain duplicated (id, wave) rows and document
  the contract: selection metrics count each pair once, output rows
  are returned as-is.

## New features

* Explicit `lower`/`upper` bounds on `weasel_plan()`: the window is
  recorded as an a-priori decision (`span_reason = "explicit"`) and
  the justification text reports it as such instead of crediting an
  automatic rule.
* Core-window selection is inspectable: every candidate window and its
  coverage is stored in `plan$span_candidates`, the objective (total
  deduplicated respondent-wave coverage, earliest window on ties) is
  documented, and exact coverage ties raise a classed warning
  (`weasel_tied_windows`).
* Qualified recommendations: `weasel_compare_scenarios()` gains
  `tie_tolerance` and a `near_tie` column; the active weights and the
  per-scenario score decomposition are attached as attributes
  (`"weights"`, `"score_components"`); documentation and generated
  sentences state that the score is a comparison-relative heuristic
  ("highest composite score under the declared weights").
* Plans record their planning population (rows and distinct ids in the
  data versus ids and unique pairs observed in the span). The
  denominator (`observed_in_span`) is printed, documented, and stated
  in the generated justification text.
* Plans store a structural fingerprint of their data;
  `weasel_apply()`, `weasel_summarize_subset()`, and
  `weasel_selectivity()` check explicitly supplied data against it and
  warn on mismatch (`weasel_data_mismatch`), guarding saved plans
  against silent reunion with the wrong data.
* The wave-pattern summary gains a stable `pattern` id column that
  survives filtering; `weasel_get_data_by_row()` accepts pattern ids
  or pattern strings, so extraction after filtering can no longer
  target the wrong pattern.
* The scope gains optional `max_missing` and `require_endpoints`
  constraints, mirroring plan scenarios exactly (locked by
  cross-pipeline equivalence tests).

## Documentation

* Guide: corrected two descriptions that contradicted the
  implementation (scope gap constraints described as inert; a pre-0.3
  complete-grid, item-NA data model with a "focal outcome" step
  presented as required), the pattern-table `n` description, the
  `weasel_example()` return listing, and the footer installation
  command (GitHub, not CRAN). All snippets start with
  `library(weasel)` and run as pasted. The plan recipes include both
  diagnostics; result notes cover recommendation semantics, candidate
  windows, explicit bounds, `keep_data` and the fingerprint guard, the
  planning population, and `grid = "observed"`. The homepage gains a
  key-definitions glossary and a "Decisions the package does not make"
  section.
* Vignettes and README synced to all of the above, including a "which
  pipeline when" rule of thumb; added `inst/CITATION` so
  `citation("weasel")` matches `CITATION.cff` with the version read
  from `DESCRIPTION` at build time.

## Testing and infrastructure

* Test suite doubled (240 to 480 assertions): cross-pipeline
  equivalence properties, algebraic invariants, brute-force agreement
  of the sensitivity sweep, row-order invariance, duplicate-contract
  regressions, serialization round trips including the legacy
  fallback, vocabulary and deprecation contracts, and snapshot tests
  freezing every user-visible textual surface.
* New `guide-snippets` workflow executes every guide code example
  against the installed package on each push; the version-coherence
  workflow additionally checks `CITATION.cff`; `.Rbuildignore` and
  `.gitignore` exclude local development artifacts (`v3/`,
  `update_github_repo.sh`), so local builds match CI builds.

# weasel 0.3.1

Coherence and reproducibility release. No behavioural changes for
correct inputs other than the RNG pinning noted below.

* `generate_weasel_dummy_data()` now pins the RNG configuration
  (`RNGkind("Mersenne-Twister", "Inversion", sample.kind = "Rejection")`,
  the R >= 3.6 defaults) inside its seed-preserving block, so a given
  `seed` reproduces the same panel regardless of the caller's
  `RNGkind()` setting. The caller's RNG state, including a non-default
  kind, is restored on exit. Previously a caller using
  `sample.kind = "Rounding"` obtained different data from the same seed.
  A regression test asserts both the invariance and the restoration.
* `NEWS.md` is now tracked in the repository. It was previously listed
  in `.gitignore`, so no changelog was under version control and the
  v0.3.0 release carried no notes.
* New `version-coherence` GitHub Actions workflow: the version displayed
  by the documentation site (`docs/index.html`) and the top `NEWS.md`
  heading must both match `Version` in `DESCRIPTION`, so the published
  site can no longer drift silently behind the released version.
* Documentation site updated to display v0.3.1.
* README: added a note describing how the package was built
  (AI-use disclosure; commit `2942bc5`, previously unreleased).

# weasel 0.3.0

This release predates the tracked `NEWS.md`; the entries below were
compiled retrospectively from the release history.

* New `grid` argument on `weasel_plan()` and `set_weasel_scope()`:
  `grid = "observed"` evaluates presence over the observed wave
  schedule, fixing a critical bug where non-consecutive schedules
  (for example biennial panels) silently returned zero respondents.
* Exports renamed with a `weasel_` prefix (`weasel_reshape_to_wide()`,
  `weasel_summarize_waves()`, `weasel_filter_wave_summary()`,
  `weasel_get_data_by_row()`, `weasel_logo()`). The old names remain as
  deprecated forwarding aliases that emit a classed warning
  (`weasel_deprecated`).
* Plan results gained a `weasel_plan` S3 class with a compact `print()`
  method, plus a `keep_data` argument so the full data set need not be
  attached to the plan object.
* New diagnostics module: `weasel_sensitivity()` (tolerance sweeps),
  `weasel_selectivity()` (retained-versus-excluded covariate comparison
  via standardized mean differences), and `weasel_scope_info()` (active
  scope state).
* Vectorized interior-gap metrics; on a 200,000 x 15 panel,
  `weasel_plan()` runtime dropped from about 10 s to about 0.9 s.
  Deduplication is order-based instead of paste-based.
* Robustness: `NA` values in the `weights` of
  `weasel_compare_scenarios()` raise an error instead of propagating
  `NA` scores; `weasel_print_table()` validates `digits` and `n`
  (rejecting `NA` and negative values) with a clean package error;
  duplicated (id, wave) rows warn once with a classed warning and are
  counted once.
* Test suite expanded to 39 test blocks; added GitHub Actions workflows
  (R CMD check and token-gated test coverage) and `LICENSE.md`.
