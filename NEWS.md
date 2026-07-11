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

Stage 4 of the 0.4.0 cycle: additive transparency and self-protection
for plans. All changes are additive; results on clean panels with
default arguments are unchanged.

* `weasel_plan()` gains explicit `lower`/`upper` bounds: the window is
  then fixed a priori (`span_reason = "explicit"`), and the generated
  justification text reports it as a design decision instead of
  attributing it to an automatic span rule. Supplying both `span` and
  bounds is an error.
* Core-window selection is now inspectable: every candidate window and
  its coverage is stored in `plan$span_candidates`, the objective
  (total deduplicated respondent-wave coverage, earliest window on
  ties) is documented, and exact coverage ties trigger a classed
  warning (`weasel_tied_windows`) instead of a silent `which.max()`.
* `weasel_compare_scenarios()` gains `tie_tolerance` and a `near_tie`
  column flagging scenarios the score cannot meaningfully separate;
  the active weights and the per-scenario score decomposition are
  attached as attributes (`"weights"`, `"score_components"`); the
  documentation now states plainly that the score is a
  comparison-relative heuristic (the size term is normalised within
  the supplied scenario set) and that endpoint-requiring scenarios
  earn the endpoint term by construction. The recommendation sentence
  is now conditional ("highest composite score under the declared
  weights") and notes when the recommendation is not unique.
* Plans now record their planning population: rows and distinct ids in
  the supplied data versus ids and unique pairs observed in the span.
  The denominator (`observed_in_span`) is printed, documented, and
  stated in the generated justification text, so retention figures can
  no longer be mistaken for proportions of the full panel.
* Plans now store a structural fingerprint of the data they were built
  from; `weasel_apply()`, `weasel_summarize_subset()`, and
  `weasel_selectivity()` compare explicitly supplied data against it
  and emit a classed warning (`weasel_data_mismatch`) when the data do
  not match, closing a silent-reunion hazard for plans saved with
  `keep_data = FALSE`.
* The wave-pattern summary gains a stable `pattern` id column that
  survives filtering, and `weasel_get_data_by_row()` accepts pattern
  ids or pattern strings. Previously the printed, filtered table gave
  no way to see the original row numbers, so extraction after
  filtering could silently target the wrong pattern.

Stage 5 of the 0.4.0 cycle: one constraint vocabulary across both
pipelines. Renames ship with deprecation paths; one default changes.

* Harmonized constraint names: `min_present`, `max_missing`,
  `n_gap_max`, `max_gap_len`, `require_endpoints`. In
  `set_weasel_scope()`, `size` becomes `min_present` (a single
  integer; the old vector form's minimum is used through the alias),
  `gap` becomes `max_gap_len`, and `n_gap` becomes `n_gap_max`. The
  deprecated aliases keep working through the 0.x series with classed
  warnings (`weasel_deprecated`), and an explicitly supplied new-name
  argument always wins over its alias.
* The scope gains optional `max_missing` and `require_endpoints`
  constraints, so the exploratory pipeline can preview exactly what a
  plan scenario would retain; the cross-pipeline equivalence tests now
  cover the anchored case too.
* Scenario tables use `max_gap_len` in place of the double-superlative
  `max_gap_max`. The old column name is accepted with a deprecation
  warning, plans saved by older versions keep working (the
  justification generator reads either column), and
  `weasel_sensitivity()` renames its argument and output column
  accordingly, keeping `max_gap_max` as a deprecated argument alias.
* The scope's default minimum presence is now `min_present = 1`:
  exploration shows every respondent with at least one observed wave
  instead of silently dropping those with fewer than three. Set
  `min_present = 3` to reproduce the previous default; the
  drop-accounting message reports exclusions either way.
* The `recommended` column keeps its name: with the declared-weights
  wording, the `near_tie` flag, and the exposed score decomposition
  from stage 4, its meaning is now explicit at the source, which is
  where the ambiguity actually lived.
* README, both vignettes, the guide, and the bundled example script
  use the harmonized vocabulary throughout.

Stage 6 of the 0.4.0 cycle: documentation enrichment and snapshot
tests. Documentation and tests only; no package code changed.

* Guide: the plan recipes now include the sensitivity and selectivity
  diagnostics; the result notes explain `recommended`/`near_tie`
  semantics, the candidate-window table, explicit a-priori bounds,
  `keep_data` with the fingerprint guard, the planning population, and
  point to `grid = "observed"` for non-consecutive schedules. The
  homepage gains a key-definitions glossary (with presence-pattern
  examples for endpoints and interior gaps) and a "Decisions the
  package does not make" section delimiting what remains the
  researcher's judgment.
* Introduction vignette: a "which pipeline when" rule of thumb, the
  stable `pattern` id, candidate windows, explicit bounds, and the
  qualified recommendation semantics. Advanced vignette: new sections
  on explicit analysis windows, auditing the recommendation (weights,
  score decomposition, near ties, comparison-relative scores), the
  planning population, and the fingerprint guard for saved plans.
* README: design-fixed window example; population and fingerprint
  noted.
* New snapshot tests freeze the user-visible textual surfaces (plan
  print, scope info, comparison and subset sentences, all three
  justification styles), so any future change to printed output shows
  up as a reviewable diff.

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
