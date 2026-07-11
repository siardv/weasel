# weasel (development version)

Stage 1 of the 0.4.0 cycle: release safety and documentation accuracy.
No changes to computational behaviour.

* Guide (documentation site): corrected two descriptions that
  contradicted the implementation. The `gap`/`n_gap` scope constraints
  were described as inert; they filter respondents at
  `weasel_reshape_to_wide()` (and are covered by tests). The
  synthetic-data walkthroughs described a pre-0.3 complete-grid,
  item-NA data model and presented a "focal outcome" filtering step as
  required; `generate_weasel_dummy_data()` produces row-absence
  missingness directly, so that step is now described only as an
  optional way to key observation to outcome availability. Also fixed:
  the pattern-table `n` column description (observed waves per pattern,
  not window length), the missing `sensitivity` element in the
  `weasel_example()` return listing, and the footer installation
  command (the package installs from GitHub, not CRAN). All guide
  code snippets now start with `library(weasel)`, so each one runs
  as pasted in a fresh session.
* `.Rbuildignore` and `.gitignore` now exclude the local `v3/` staging
  folder and `update_github_repo.sh`, so tarballs built from a local
  working copy match CI builds, no development artifacts ship, and an
  accidental `git add -A` cannot publish internal drafts.
* Added `inst/CITATION` so `citation("weasel")` agrees with
  `CITATION.cff`; the version is read from `DESCRIPTION` at build time.
* The version-coherence workflow now also checks the `CITATION.cff`
  version against `DESCRIPTION`.

Stage 2 of the 0.4.0 cycle: test scaffolding and CI hardening.
Additive, plus one bug the new scaffolding exposed immediately.

* Fixed: the backwards-compatibility fallback for plan objects saved
  by pre-0.3 versions (which stored only the span bounds, not `$span`)
  had never worked. `$` partial matching picked up `span_reason`
  instead, its `"full"`/`"core"` label coerced to `NA`, and every wave
  failed the span filter, so `weasel_apply()`,
  `weasel_summarize_subset()`, and `weasel_selectivity()` silently
  returned zero rows for such objects. Exact `[["span"]]` indexing in
  `.weasel_plan_span()` and `print.weasel_plan()` fixes this; a
  regression test now exercises the legacy path end to end.

* New test suites lock the package's central methodological claims in
  place before any further changes: cross-pipeline equivalence (the
  scope constraints and an equivalent plan scenario must retain
  identical respondents, on consecutive and observed grids, with
  identical per-respondent metrics), algebraic invariants on the id
  metrics and pattern tables, exact agreement between
  `weasel_sensitivity()` and brute-force filtering, row-order
  invariance of plans, duplicate-row neutrality in both pipelines, and
  serialization round trips (including `keep_data = FALSE` reunion and
  the pre-0.3 legacy fallback for plans without a stored `$span`).
* New `guide-snippets` GitHub Actions workflow: every R code snippet
  embedded in the guide is extracted and executed against the
  installed package on each push, so guide examples can no longer
  drift from the implementation without failing CI.

Stage 3 of the 0.4.0 cycle: dirty-input and degenerate-case
correctness. Behaviour changes affect malformed input and degenerate
cases only; results on clean panels are unchanged (and guarded by the
stage 2 invariant and equivalence suites).

* `weasel_compare_to_sentence()` no longer fabricates a
  recommendation when no scenario is recommendable (previously it fell
  back to the first row and reported it as recommended); it now states
  plainly that no scenario is recommended.
* `weasel_selectivity()` now deduplicates (id, wave) pairs: covariate
  values are averaged within each duplicated pair, a classed warning
  (`weasel_duplicates`) is emitted, and the diagnostic no longer
  depends on the row order of the input. Previously, with duplicated
  rows, `at = "first"` silently used whichever duplicate appeared
  first and `at = "mean"` double-counted duplicated waves.
* `weasel_apply()` and `weasel_summarize_subset()` emit a classed
  warning (`weasel_duplicates`) when the returned data still contain
  duplicated (id, wave) rows, and their documentation now spells out
  the contract: selection metrics count each pair once, output rows
  are returned as-is (participation deduplication is not output-row
  deduplication).
* `id` and `wave` must now name different columns; previously
  `weasel_plan(d, "time", "time")` was accepted and silently retained
  nobody.
* Rows excluded from participation analysis (missing id, missing
  wave, outside the span) are now counted in a verbose-mode message in
  both pipelines instead of disappearing silently.
* `weasel_justify_subset()` refuses to write a justification for a
  scenario that retains no respondents (classed error
  `weasel_error_empty_scenario`, also added to the corresponding
  errors in `weasel_summarize_subset()`).
* Scenario matching now accepts an exact name or an unambiguous
  prefix; arbitrary substrings (for example `"strict"` for
  `anchored_strict`, or `"ed_ba"`) no longer match.
* Strict integer validation everywhere a whole number is expected:
  `gap`, `n_gap`, `size`, `lower`, `upper`, `core_len`, sensitivity
  tolerances, scenario-table tolerance columns (`Inf` still means "no
  constraint"), `digits`/`n` in `weasel_print_table()`, and the
  generator's count and `waves` arguments. Fractional values are
  rejected with a clear error instead of being silently truncated
  (previously `gap = 1.9` acted as `gap = 1`) or rounded (previously
  `lower = 2.6` acted as `lower = 3`).

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
