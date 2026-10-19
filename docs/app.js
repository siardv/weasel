/* weasel decision guide: tree data + ui controller */

const REAL_DATA_GUIDANCE =
  'Before running this recipe, set `wave_grid` deliberately: use `"consecutive"` ' +
  'when every integer in the window is scheduled, or `"observed"` when the distinct ' +
  'recorded wave values represent the intended schedule. An entirely absent scheduled ' +
  'wave is lost from an observed grid. Inspect the printed span before selection. ';

const TREE = {
  start: {
    type: "question",
    shortLabel: "Own data",
    question:
      "Do you have your own longitudinal panel data ready to work with?",
    explanation:
      "Both pipelines expect a long-format data frame (one row per respondent per wave) " +
      "with a respondent identifier column and a numeric, integer-valued wave column. " +
      "Review the data-readiness guidance above and choose the scheduled grid before copying a recipe. " +
      "A wave is treated as observed when a row for that respondent and wave exists; " +
      "a missed wave is simply an absent row, which is how real panel files usually arrive. " +
      "If you do not have real data yet, weasel can generate a synthetic panel so you can " +
      "learn or prototype a workflow first.",
    yes: { label: "Yes, I have my own data", next: "goal" },
    no: { label: "No, I need example or dummy data", next: "dummy_purpose" },
  },

  goal: {
    type: "question",
    shortLabel: "Exploratory goal",
    question:
      "Do you want to explore participation patterns before deciding on a selection rule?",
    explanation:
      "Scope maps participation fingerprints and extracts respondents under one active " +
      "configuration. It also enforces structural constraints such as endpoint, missing-wave " +
      "and interior-gap requirements. Plan compares and records named alternatives under " +
      "those constraints. Use Scope to inspect patterns under one configuration, or Plan " +
      "to compare alternative rules. Both pipelines can enforce selection criteria.",
    yes: { label: "Yes, I want to map patterns first", next: "scope_range" },
    no: {
      label: "No, I want to compare selection rules",
      next: "plan_criteria",
    },
  },

  dummy_purpose: {
    type: "question",
    shortLabel: "Full demo",
    question:
      "Do you want a single guided demonstration that covers the entire package?",
    explanation:
      "`weasel_example()` runs both pipelines on a 1,000-respondent synthetic panel, prints " +
      "all intermediate outputs, and returns a named list of every key object. " +
      "It is the fastest way to verify your installation and see what the package produces. " +
      "If you would rather explore one pipeline in depth, stepping through each function " +
      "yourself, answer No.",
    yes: { label: "Yes, run the full demonstration", next: "result_demo" },
    no: { label: "No, I want to explore one pipeline", next: "dummy_goal" },
  },

  dummy_goal: {
    type: "question",
    shortLabel: "Dummy pipeline",
    question:
      "Do you want to explore the pattern-exploration pipeline (Scope) rather than the scenario-comparison pipeline (Plan)?",
    explanation:
      "The Scope pipeline focuses on understanding what patterns exist in the data: it groups " +
      "respondents by their wave fingerprint and lets you extract subsets for any pattern. " +
      "The Plan pipeline focuses on a different question: given a set of structural requirements, " +
      "which respondents qualify, and what is the trade-off between stringency and sample size?",
    yes: {
      label: "Yes, explore the Scope pipeline",
      next: "result_scope_learn",
    },
    no: { label: "No, explore the Plan pipeline", next: "result_plan_learn" },
  },

  scope_range: {
    type: "question",
    shortLabel: "Wave range",
    question: "Do you want to restrict the analysis to a specific wave range?",
    explanation:
      "Supplying an upper (and optionally a lower) bound to `set_weasel_scope()` limits " +
      "the presence matrix to a meaningful window. This makes the pattern table more " +
      "interpretable and avoids artefacts from sparse early or late waves. " +
      "If you are uncertain, a conservative upper bound is a sensible default; " +
      "you can always widen the window and re-run.",
    yes: {
      label: "Yes, I will set an upper or lower bound",
      next: "result_scope_bounded",
    },
    no: {
      label: "No, I want all available waves included",
      next: "result_scope_full",
    },
  },

  plan_criteria: {
    type: "question",
    shortLabel: "Selection criteria",
    question: "Do you want to enter your own named selection criteria?",
    explanation:
      "Plan accepts a table of named alternatives with endpoint, missing-wave and interior-gap " +
      "requirements. The three built-in presets are examples to review against your study " +
      "design, not rules that every analysis should adopt. If you already have criteria, " +
      "use the custom-table recipe; if you want to inspect the built-in examples first, " +
      "continue to the preset recipes. For one active configuration, Scope also enforces " +
      "structural constraints without comparing alternatives.",
    yes: { label: "Yes, show a custom-criteria table", next: "result_plan_custom" },
    no: { label: "No, compare the built-in examples first", next: "plan_doc" },
  },

  plan_doc: {
    type: "question",
    shortLabel: "Methods documentation",
    question:
      "Do you need to formally document the selection in a methods section or pre-registration?",
    explanation:
      "`weasel_justify_subset()` generates a structured paragraph (in methods, concise, or " +
      "extended style) that documents the window bounds, structural constraints, resulting " +
      "sample size, and key coverage statistics, with an automatic in-text citation. " +
      "If you are writing a manuscript, registered report, or pre-registration where the " +
      "data-preparation stage must be reported transparently, answer Yes.",
    yes: {
      label: "Yes, I need a methods-section paragraph",
      next: "plan_span_justify",
    },
    no: { label: "No, documentation is not required", next: "plan_span" },
  },

  plan_span_justify: {
    type: "question",
    shortLabel: "Common analysis window",
    question:
      "Do you want a common fixed-length analysis window?",
    explanation:
      "`span = \"core\"` asks weasel to locate the highest-coverage window of consecutive grid waves of " +
      "a specified length (`core_len`, default 6). A common window does not mean every " +
      "respondent is observed at every wave: balanced and lenient scenarios allow missing " +
      "waves. Complete participation requires `max_missing = 0`, as in `anchored_strict`. " +
      "This counts row presence, not non-missing outcomes. `span = \"full\"` uses the full " +
      "wave range; completeness still depends on the scenario. To supply your own window, " +
      "skip `span` and pass explicit `lower`/`upper` bounds instead. The justification text " +
      "describes the bounds as user-supplied, without inferring when or why they were chosen.",
    yes: {
      label: "Yes, I need a fixed-length window",
      next: "result_plan_justify_core",
    },
    no: {
      label: "No, I want all available waves",
      next: "result_plan_justify_full",
    },
  },

  plan_span: {
    type: "question",
    shortLabel: "Common analysis window",
    question: "Do you want a common fixed-length analysis window?",
    explanation:
      "`span = \"core\"` selects the densest window of a fixed number of consecutive grid waves. " +
      "Every selected respondent shares that window, but balanced and lenient scenarios " +
      "allow missing waves within it. Complete participation requires `max_missing = 0`, " +
      "as in `anchored_strict`; row presence does not ensure non-missing outcomes. " +
      "`span = \"full\"` covers the full wave range, with completeness again determined " +
      "by the scenario. A window fixed " +
      "by the study design can be supplied directly as explicit `lower`/`upper` bounds " +
      "instead of `span`.",
    yes: {
      label: "Yes, I need a fixed-length window",
      next: "result_plan_core",
    },
    no: { label: "No, I want all available waves", next: "result_plan_full" },
  },

  // RESULT NODES

  result_scope_full: {
    type: "result",
    pipeline: "Scope Pipeline",
    badgeClass: "badge-scope",
    heading: "Scope pipeline: full wave range",
    summary: REAL_DATA_GUIDANCE +
      "Explore all waves without restriction. The presence matrix covers the complete " +
      "observed range, giving an unfiltered view of participation patterns across every " +
      "wave in your panel. This is the appropriate starting point when you have no " +
      "prior knowledge of where missingness is concentrated.",
    steps: [
      {
        fn: 'set_weasel_scope(data, "id", "time", grid = wave_grid)',
        note: "initialise the scope environment; no bounds means weasel uses the full observed range",
      },
      {
        fn: "evaluate_weasel_scope()",
        note: "resolve lower and upper bounds from the data; inspect them before pivoting if needed",
      },
      {
        fn: "weasel_scope_info()$span",
        note: "inspect the actual scheduled wave vector before reshaping or selection",
      },
      {
        fn: "weasel_reshape_to_wide()",
        note: "pivot from long to a respondent by wave presence matrix; respondents with too few observed waves are dropped",
      },
      {
        fn: "weasel_summarize_waves()",
        note: "group respondents by their wave fingerprint and count how many share each pattern",
      },
      {
        fn: "weasel_filter_wave_summary()",
        note: "inspect the pattern table; use `ids_range = c(N, Inf)` to focus on frequent patterns",
      },
      {
        fn: "weasel_get_data_by_row(i)",
        note: "extract long-format data for every respondent matching pattern row i",
      },
    ],
    code: `library(weasel)

d <- your_data  # long-format: id, time, var1 ...

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

set_weasel_scope(d, "id", "time", grid = wave_grid)
evaluate_weasel_scope()
print(weasel_scope_info()$span)
weasel_reshape_to_wide()
weasel_summarize_waves()

weasel_print_table(weasel_filter_wave_summary(), title = "Wave patterns")

subset1 <- weasel_get_data_by_row(1)
head(subset1)`,
    note:
      "In the pattern table, `waves` is the participation fingerprint (a '.' marks a missing " +
      "wave), `n` is the number of observed waves in that pattern, and `ids` counts the " +
      "respondents who share it. Structural constraints (`min_present`, `max_missing`, " +
      "`max_gap_len`, `n_gap_max`, `require_endpoints`) are enforced when " +
      "`weasel_reshape_to_wide()` runs: respondents outside the limits are dropped and a " +
      "status message reports how many. By default `min_present = 1`, so every respondent " +
      "with at least one observed wave is shown. The `pattern` column is a stable row id: " +
      "filter the table however you like, then pass a `pattern` value (or the `waves` string " +
      "itself) to `weasel_get_data_by_row()`. To compare several tolerance settings side by " +
      "side instead of committing to one, use the Plan pipeline.",
  },

  result_scope_bounded: {
    type: "result",
    pipeline: "Scope Pipeline",
    badgeClass: "badge-scope",
    heading: "Scope pipeline: bounded wave range",
    summary: REAL_DATA_GUIDANCE +
      "Focus on a specific wave window. Bounding the range with `upper` (and optionally `lower`) " +
      "keeps the pattern table interpretable and removes sparse early or late waves. " +
      "Respondents who skip waves within the window are retained; their gaps show up as " +
      "distinct fingerprints (the '.' marks) rather than being filtered out.",
    steps: [
      {
        fn: 'set_weasel_scope(data, "id", "time", upper = N, grid = wave_grid)',
        note: "restrict to waves 1 through N; add `lower = M` for a non-zero start",
      },
      {
        fn: "evaluate_weasel_scope()",
        note: "finalise and validate the declared bounds",
      },
      {
        fn: "weasel_scope_info()$span",
        note: "inspect the actual scheduled wave vector before reshaping or selection",
      },
      { fn: "weasel_reshape_to_wide()", note: "build the restricted presence matrix" },
      {
        fn: "weasel_summarize_waves()",
        note: "enumerate distinct participation patterns within the window",
      },
      {
        fn: "weasel_filter_wave_summary(ids_range = c(5, Inf))",
        note: "optional: keep only patterns shared by at least 5 respondents (`ids` counts respondents; `n` counts observed waves)",
      },
      {
        fn: "weasel_get_data_by_row(common$pattern[1])",
        note: "extract by stable pattern id: `pattern` values survive filtering, so this is exactly the pattern inspected above, never a display row position",
      },
    ],
    code: `library(weasel)

d <- your_data

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

set_weasel_scope(d, "id", "time", upper = 10, grid = wave_grid)
evaluate_weasel_scope()
print(weasel_scope_info()$span)
weasel_reshape_to_wide()
weasel_summarize_waves()

common <- weasel_filter_wave_summary(ids_range = c(5, Inf))
weasel_print_table(common, title = "Common patterns (ids >= 5)")

if (nrow(common) > 0) {
  subset1 <- weasel_get_data_by_row(common$pattern[1])
}`,
    note:
      "`max_gap_len` and `n_gap_max` are enforced at reshape time inside the bounded window: " +
      "interior gaps are measured strictly between a respondent's first and last observed " +
      "wave within the span, so late entry and early exit never count as gaps. Respondents " +
      "exceeding the limits are dropped with a status message. Extraction goes through the " +
      "stable `pattern` id of the named, filtered table (`common$pattern[1]`), not through a " +
      "display row position: after filtering, row 1 of the visible table and pattern 1 of the " +
      "full summary are different concepts, and the `nrow(common) > 0` guard makes the " +
      "empty-filter case explicit instead of silently extracting an unrelated pattern. The " +
      "filter is on `ids` (respondents sharing a pattern), not `n` (observed waves in the " +
      "pattern). The same constraint names are used by plan scenario tables; to weigh several " +
      "tolerances against each other before committing, use the Plan pipeline " +
      "(`weasel_plan()` scenarios or `weasel_sensitivity()`).",
  },

  result_scope_learn: {
    type: "result",
    pipeline: "Scope Pipeline",
    badgeClass: "badge-scope",
    heading: "Scope pipeline: learning with synthetic data",
    summary:
      "Generate a synthetic panel and walk through the Scope pipeline step by step. " +
      "The generator produces long-format data in which a missed wave is an absent row, " +
      "exactly how the package defines observation, by layering random skips, attention " +
      "decay, permanent attrition, and block dropout, so realistic participation patterns " +
      "are visible immediately.",
    steps: [
      {
        fn: "generate_weasel_dummy_data(n_ids = 200, seed = 42)",
        note: "create a 200-respondent by 13-wave panel; fix the seed for reproducibility",
      },
      {
        fn: 'set_weasel_scope(d, "id", "time", upper = 10)',
        note: "initialise the scope environment; upper = 10 restricts to the first 10 waves",
      },
      {
        fn: "evaluate_weasel_scope()",
        note: "resolve bounds and valid window sizes",
      },
      {
        fn: "weasel_reshape_to_wide()",
        note: "pivot to the respondent by wave presence matrix",
      },
      {
        fn: "weasel_summarize_waves()",
        note: "enumerate participation patterns across respondents",
      },
      {
        fn: "weasel_filter_wave_summary()",
        note: "view the pattern table; try `ids_range = c(5, Inf)` to focus on common patterns",
      },
      {
        fn: "weasel_get_data_by_row(1)",
        note: "extract long-format data for the most common pattern",
      },
    ],
    code: `library(weasel)

# a missed wave is an absent row, as in real long-format panel files
d <- generate_weasel_dummy_data(n_ids = 200, seed = 42)

set_weasel_scope(d, "id", "time", upper = 10)
evaluate_weasel_scope()
weasel_reshape_to_wide()
weasel_summarize_waves()

weasel_print_table(weasel_filter_wave_summary(), title = "Wave patterns", n = 10)

subset1 <- weasel_get_data_by_row(1)
head(subset1)`,
    note:
      "Item nonresponse (`NA` values inside outcome columns of observed rows, controlled by " +
      "`prop_item_missing`) is distinct from wave-level missingness and never affects " +
      "selection: participation is defined by row presence alone. If your research question " +
      "requires keying observation to outcome availability instead, filter first " +
      "(`d <- d[!is.na(d$var1), ]`) and state that redefinition explicitly in your methods " +
      "text. For biennial or other non-consecutive schedules, generate with " +
      "`waves = seq(2008, 2020, 2)` and analyse with `grid = \"observed\"`.",
  },

  result_plan_custom: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: custom selection criteria",
    summary: REAL_DATA_GUIDANCE +
      "Enter your design requirements as a table of named alternatives. This example " +
      "compares complete participation with at most one missed interior wave, requiring " +
      "both endpoints in each case. Replace these example tolerances with your own " +
      "reviewed criteria. The example deliberately chooses `custom_one_missing`; the " +
      "comparison score does not choose the study's inclusion rule for you.",
    steps: [
      {
        fn: "criteria <- data.frame(scenario, require_endpoints, max_missing, n_gap_max, max_gap_len)",
        note: "define named alternatives using the five supported Plan fields; review every example value",
      },
      {
        fn: 'weasel_plan(data, "id", "time", span = "full", grid = wave_grid, scenarios = criteria)',
        note: "evaluate your alternatives on the declared grid; use explicit bounds instead of span for a design-fixed window",
      },
      {
        fn: "p$span",
        note: "inspect the resolved scheduled waves before selection",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "inspect retained counts and trade-offs; the score is a comparison-relative heuristic",
      },
      {
        fn: 'weasel_summarize_subset(p, chosen_scenario, data, "id", "time")',
        note: "check that the deliberately chosen named rule retains respondents, then audit coverage",
      },
      {
        fn: "weasel_apply(p, chosen_scenario)",
        note: "extract exactly the sample described by the audit and methods paragraph",
      },
      {
        fn: "weasel_justify_subset(p, chosen_scenario)",
        note: "report the recorded constraints and selection; add your own rationale for choosing them",
      },
    ],
    code: `library(weasel)

d <- your_data

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

# example alternatives only: replace with your study's reviewed criteria
criteria <- data.frame(
  scenario = c("custom_complete", "custom_one_missing"),
  require_endpoints = c(TRUE, TRUE),
  max_missing = c(0L, 1L),
  n_gap_max = c(0L, 1L),
  max_gap_len = c(0L, 1L)
)
p <- weasel_plan(d, "id", "time", span = "full",
  grid = wave_grid, scenarios = criteria)
print(p$span)
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Custom scenario comparison")

chosen_scenario <- "custom_one_missing"  # deliberate example choice; inspect cmp first
chosen_row <- match(chosen_scenario, p$plan$scenario)
if (length(chosen_scenario) != 1L || is.na(chosen_row) ||
    p$plan$n_ids[chosen_row] == 0L) {
  stop("The chosen custom scenario is absent or empty. Inspect cmp and deliberately revise the window or criteria before continuing.")
}
s <- weasel_summarize_subset(p, chosen_scenario, d, "id", "time")
weasel_print_table(s$headline)
weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")

analysis_data <- weasel_apply(p, chosen_scenario)
dim(analysis_data)
cat(weasel_justify_subset(p, chosen_scenario), "\\n")`,
    note:
      "Plan uses exactly the five fields shown; `min_present` is Scope-only. On a fixed " +
      "grid of length L, at least M observed waves is `max_missing = L - M` for integer " +
      "1 <= M <= L, with other constraints unchanged. A `custom_one_missing` sample can " +
      "contain incomplete respondents; complete participation requires `max_missing = 0`. " +
      "These are row-presence requirements, not outcome-completeness requirements. " +
      "Scope can enforce one such configuration too; Plan records the named alternatives " +
      "for comparison. If your chosen rule retains nobody, the recipe stops before summary " +
      "or extraction so you can revise it deliberately.",
  },

  result_plan_core: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: core window",
    summary: REAL_DATA_GUIDANCE +
      "weasel locates the highest-coverage window of consecutive grid waves of length `core_len` " +
      "(default 6), then evaluates three built-in scenarios: `anchored_strict` (no missing " +
      "waves, both endpoints required), `anchored_balanced` (at most 1 missing, both endpoints), " +
      "and `lenient_info_max` (at most 2 missing, endpoints not required), scoring each on " +
      "coverage, endpoint rate, and gap structure. These presets are examples, not study-design " +
      "requirements. Review the scored table and choose the " +
      "scenario that best fits your analytical requirements. A shared window does not " +
      "ensure complete participation: only the strict preset requires every wave.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "core", grid = wave_grid)',
        note: "build and score three scenarios over the highest-coverage window of consecutive grid waves; returns a plan object",
      },
      {
        fn: "p$span",
        note: "inspect the resolved scheduled waves before comparing or selecting respondents",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "inspect the recommendation; stop if none retains respondents, then deliberately reconsider the window or criteria",
      },
      {
        fn: 'weasel_summarize_subset(p, chosen_scenario, data, "id", "time")',
        note: "audit per-wave coverage and missingness distribution before extracting data",
      },
      {
        fn: "weasel_apply(p, chosen_scenario)",
        note: "produce the filtered long-format data frame for the chosen scenario",
      },
      {
        fn: "weasel_sensitivity(p, require_endpoints = TRUE, max_missing = 0:2, n_gap_max = 1, max_gap_len = 1)",
        note: "one-factor sweep: vary missing-wave tolerance while holding endpoint and gap requirements fixed",
      },
      {
        fn: "weasel_selectivity(p, chosen_scenario, vars = diagnostic_vars)",
        note: "optional: deliberately choose eligible numeric/logical covariates and check that retained and excluded groups support a comparison",
      },
    ],
    code: `library(weasel)

d <- your_data

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

p   <- weasel_plan(d, "id", "time", span = "core", grid = wave_grid)
print(p$span)
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenario comparison")
cat(weasel_compare_to_sentence(cmp), "\\n")

chosen_scenario <- cmp$scenario[which(cmp$recommended %in% TRUE & cmp$n_ids > 0)]
if (length(chosen_scenario) != 1L || is.na(chosen_scenario) ||
    !chosen_scenario %in% p$plan$scenario) {
  stop("No single recommended scenario. Inspect cmp and deliberately revise the window or criteria before continuing.")
}
s <- weasel_summarize_subset(p, chosen_scenario, d, "id", "time")
weasel_print_table(s$headline)
weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")

analysis_data <- weasel_apply(p, chosen_scenario)
dim(analysis_data)

# one-factor sweep: hold endpoint and gap requirements fixed
sens <- weasel_sensitivity(p, require_endpoints = TRUE,
  max_missing = 0:2, n_gap_max = 1, max_gap_len = 1)
weasel_print_table(sens, title = "Missing-wave tolerance (other requirements fixed)")

# optional: replace character(0) with deliberately chosen covariate names
diagnostic_vars <- character(0)
chosen_ids <- p$plan$ids[[match(chosen_scenario, p$plan$scenario)]]
excluded_ids <- setdiff(p$id_metrics[[p$id]], chosen_ids)
if (length(chosen_ids) < 2L || length(excluded_ids) < 2L) {
  message("Selectivity skipped: need at least two retained and two excluded respondents.")
} else if (length(diagnostic_vars) == 0L) {
  message("Selectivity skipped: choose numeric/logical covariates to compare.")
} else {
  eligible <- setdiff(names(d)[vapply(d, function(x) is.numeric(x) || is.logical(x),
    logical(1))], c(p$id, p$wave))
  if (!all(diagnostic_vars %in% eligible)) {
    stop("Choose numeric/logical covariates other than the id and wave columns.")
  }
  sel <- weasel_selectivity(p, chosen_scenario, vars = diagnostic_vars, at = "first")
  weasel_print_table(sel, digits = 3)
}`,
    note:
      "`recommended` marks the highest composite score under the declared weights, nothing " +
      "more: check the `near_tie` column and `attr(cmp, \"score_components\")` before treating " +
      "it as a decision, and inspect `p$span_candidates` to see every window the core rule " +
      "considered (exact coverage ties warn and pick the earliest). For a design-fixed window pass explicit " +
      "`lower`/`upper` bounds instead of `span`. If no scenario retains anyone, the code " +
      "stops before summary or extraction; inspect the table and deliberately reconsider " +
      "the window or criteria. Selectivity is optional: set `diagnostic_vars` only after " +
      "choosing relevant numeric/logical covariates. `at = \"first\"` uses each respondent's " +
      "first observed wave in the span, not necessarily a shared baseline. Inspect the " +
      "per-covariate counts: missing summaries can reduce them, and an SMD needs at least " +
      "two non-missing summaries per group and a non-zero pooled standard deviation.",
  },

  result_plan_full: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: full span",
    summary: REAL_DATA_GUIDANCE +
      "The full resolved grid is included in the evaluation window. The three built-in " +
      "presets are examples to review against your design. Scenarios are scored " +
      "against the complete panel range. Use this when the full range is your intended " +
      "analysis window. This does not require complete observation at every wave: " +
      "balanced and lenient scenarios allow missing waves; `anchored_strict` requires " +
      "complete participation through `max_missing = 0`.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "full", grid = wave_grid)',
        note: "evaluate all three default scenarios across every resolved grid wave",
      },
      {
        fn: "p$span",
        note: "inspect the resolved scheduled waves before comparing or selecting respondents",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "inspect the recommendation; stop if none retains respondents, then deliberately reconsider the window or criteria",
      },
      {
        fn: 'weasel_summarize_subset(p, chosen_scenario, data, "id", "time")',
        note: "verify per-wave coverage and missingness statistics for the chosen scenario",
      },
      {
        fn: "weasel_apply(p, chosen_scenario)",
        note: "extract the analysis-ready long-format data frame",
      },
    ],
    code: `library(weasel)

d <- your_data

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

p   <- weasel_plan(d, "id", "time", span = "full", grid = wave_grid)
print(p$span)
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Full-span scenarios")
cat(weasel_compare_to_sentence(cmp), "\\n")

chosen_scenario <- cmp$scenario[which(cmp$recommended %in% TRUE & cmp$n_ids > 0)]
if (length(chosen_scenario) != 1L || is.na(chosen_scenario) ||
    !chosen_scenario %in% p$plan$scenario) {
  stop("No single recommended scenario. Inspect cmp and deliberately revise the window or criteria before continuing.")
}

s <- weasel_summarize_subset(p, chosen_scenario, d, "id", "time")
cat(weasel_subset_to_sentence(s), "\\n")

analysis_data <- weasel_apply(p, chosen_scenario)
dim(analysis_data)

# one-factor sweep: hold endpoint and gap requirements fixed
sens <- weasel_sensitivity(p, require_endpoints = TRUE,
  max_missing = 0:2, n_gap_max = 1, max_gap_len = 1)
weasel_print_table(sens, title = "Missing-wave tolerance (other requirements fixed)")`,
    note:
      "Plans carry their data by default; use `keep_data = FALSE` before saving with " +
      "`saveRDS()` and pass the data back in later. Reunions are guarded: explicitly supplied " +
      "data are checked against the plan's structural fingerprint, and a mismatch warns " +
      "(`weasel_data_mismatch`). The printed plan reports the planning population " +
      "(respondents observed in the span versus all distinct ids in the data), which is the " +
      "denominator behind every retention figure. Review the grid against the schedule " +
      "before interpreting those figures.",
  },

  result_plan_justify_core: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: core window with methods justification",
    summary: REAL_DATA_GUIDANCE +
      "Build a scenario plan over the highest-coverage window of consecutive grid waves, " +
      "select a scenario, and generate a structured paragraph for your methods section. " +
      "`weasel_justify_subset()` produces text in three verbosity styles and adds a " +
      "formal in-text citation automatically. Designed for pre-registered studies and " +
      "manuscripts where the data-preparation stage must be documented transparently. " +
      "A fixed window does not ensure complete records: balanced and lenient scenarios " +
      "allow missing waves; `anchored_strict` uses `max_missing = 0` for complete participation.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "core", grid = wave_grid)',
        note: "score scenarios over the best-coverage window of consecutive grid waves; window bounds are stored in the plan object",
      },
      {
        fn: "p$span",
        note: "inspect the resolved scheduled waves before comparing or selecting respondents",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "inspect the recommendation as a starting point; stop if none retains respondents and deliberately reconsider the window or criteria",
      },
      {
        fn: 'weasel_summarize_subset(p, chosen_scenario, data, "id", "time")',
        note: "inspect headline statistics, per-wave coverage, and missingness distribution before committing",
      },
      {
        fn: "weasel_apply(p, chosen_scenario)",
        note: "extract the filtered long-format data frame",
      },
      {
        fn: "weasel_justify_subset(p, chosen_scenario)",
        note: 'generate the methods paragraph; `style` controls verbosity: `"methods"` (default), `"concise"`, or `"extended"`',
      },
    ],
    code: `library(weasel)

d <- your_data

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

p   <- weasel_plan(d, "id", "time", span = "core", grid = wave_grid)
print(p$span)
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenario comparison")

chosen_scenario <- cmp$scenario[which(cmp$recommended %in% TRUE & cmp$n_ids > 0)]
if (length(chosen_scenario) != 1L || is.na(chosen_scenario) ||
    !chosen_scenario %in% p$plan$scenario) {
  stop("No single recommended scenario. Inspect cmp and deliberately revise the window or criteria before continuing.")
}

s <- weasel_summarize_subset(p, chosen_scenario, d, "id", "time")
weasel_print_table(s$headline)

analysis_data <- weasel_apply(p, chosen_scenario)

# full methods-section paragraph; the in-text citation
# (package author and year) is added automatically
cat(weasel_justify_subset(p, chosen_scenario), "\\n")

# short summary for supplementary material
cat(weasel_justify_subset(p, chosen_scenario, style = "concise"), "\\n")

# without the citation
cat(weasel_justify_subset(p, chosen_scenario, cite = FALSE), "\\n")`,
    note:
      'Three verbosity styles are available: `"methods"` (full paragraph, default), ' +
      '`"concise"` (short summary, suitable for supplementary sections), and ' +
      '`"extended"` (detailed rationale). ' +
      "The in-text citation is derived automatically from the package metadata, " +
      "for example `(van den Bosch, 2026)`; supply `author` and `year` to override " +
      "it, or `cite = FALSE` to omit it.",
  },

  result_plan_justify_full: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: full span with methods justification",
    summary: REAL_DATA_GUIDANCE +
      "Evaluate scenarios over all available waves and produce a fully documented, " +
      "reproducible selection. The paragraph from `weasel_justify_subset()` will describe " +
      "the full observed wave range, the structural constraints applied, and the resulting " +
      "sample-size and coverage statistics. Balanced and lenient scenarios allow missing " +
      "waves within that range; complete participation requires `max_missing = 0`, as in " +
      "`anchored_strict`.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "full", grid = wave_grid)',
        note: "score all three scenarios over the complete wave range",
      },
      {
        fn: "p$span",
        note: "inspect the resolved scheduled waves before comparing or selecting respondents",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "inspect the recommendation; stop if none retains respondents, then deliberately reconsider the window or criteria",
      },
      {
        fn: 'weasel_summarize_subset(p, chosen_scenario, data, "id", "time")',
        note: "verify coverage statistics for the chosen scenario",
      },
      {
        fn: "weasel_apply(p, chosen_scenario)",
        note: "produce the analysis-ready data frame",
      },
      {
        fn: "weasel_justify_subset(p, chosen_scenario)",
        note: "generate the methods paragraph documenting window bounds, constraints, and statistics",
      },
    ],
    code: `library(weasel)

d <- your_data

# choose from the study schedule; use "observed" only when it represents that schedule
wave_grid <- "consecutive"

p   <- weasel_plan(d, "id", "time", span = "full", grid = wave_grid)
print(p$span)
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Full-span scenarios")

chosen_scenario <- cmp$scenario[which(cmp$recommended %in% TRUE & cmp$n_ids > 0)]
if (length(chosen_scenario) != 1L || is.na(chosen_scenario) ||
    !chosen_scenario %in% p$plan$scenario) {
  stop("No single recommended scenario. Inspect cmp and deliberately revise the window or criteria before continuing.")
}

analysis_data <- weasel_apply(p, chosen_scenario)

# the citation (package author and year) is included automatically
cat(weasel_justify_subset(p, chosen_scenario), "\\n")`,
    note:
      "The in-text citation is automatic; supply `author` and `year` only to " +
      'override it, or `cite = FALSE` to omit it. Style options: `"methods"` ' +
      '(default), `"concise"`, and `"extended"`.',
  },

  result_plan_learn: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: learning with synthetic data",
    summary:
      "Generate a dummy panel and walk through the complete Plan pipeline: build scenarios, " +
      "inspect the scored comparison table, audit your preferred choice with per-wave " +
      "coverage and missingness statistics, and extract the filtered data. The generator's " +
      "layered wave-level missingness produces genuine trade-offs between stringency and " +
      "sample size.",
    steps: [
      {
        fn: "generate_weasel_dummy_data(n_ids = 100, seed = 1)",
        note: "create a 100-respondent synthetic panel with layered item-level missingness",
      },
      {
        fn: 'weasel_plan(d, "id", "time", span = "core")',
        note: "build and score three scenarios; the plan object stores respondent metrics and window bounds",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "compute composite scores and flag the recommended scenario",
      },
      {
        fn: 'weasel_summarize_subset(p, chosen_scenario, d, "id", "time")',
        note: "audit headline statistics, per-wave coverage, and the missingness distribution",
      },
      {
        fn: 'weasel_apply(p, chosen_scenario)',
        note: "extract the filtered long-format data frame",
      },
    ],
    code: `library(weasel)

# wave-level missingness is genuine: a missed wave is an absent row
d <- generate_weasel_dummy_data(n_ids = 100, seed = 1)

p   <- weasel_plan(d, "id", "time", span = "core")
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenario comparison")
cat(weasel_compare_to_sentence(cmp), "\\n")

chosen_scenario <- "anchored_balanced"  # deliberate example choice; inspect cmp first
s <- weasel_summarize_subset(p, chosen_scenario, d, "id", "time")
weasel_print_table(s$headline, title = "Subset headline")
weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")
cat(weasel_subset_to_sentence(s), "\\n")

balanced <- weasel_apply(p, chosen_scenario)
dim(balanced)`,
    note:
      "The three default scenarios retain different samples because the synthetic panel " +
      "contains genuine wave-level missingness: stricter scenarios keep fewer respondents " +
      "at higher within-window completeness. Item nonresponse inside observed rows does not " +
      "affect selection; see the Scope walkthrough note if you need observation keyed to " +
      "outcome availability.",
  },

  result_demo: {
    type: "result",
    pipeline: "Full Demonstration",
    badgeClass: "badge-demo",
    heading: "Full demonstration: weasel_example()",
    summary:
      "`weasel_example()` exercises both the Scope and Plan pipelines on a synthetic " +
      "1,000-respondent panel, printing all intermediate outputs to the console and " +
      "returning a named list of every key object. It is the fastest way to verify your " +
      "installation and see the shape of what the package produces end-to-end.",
    steps: [
      {
        fn: "weasel_example(seed = 42)",
        note: "run the complete demonstration; returns a named list invisibly: `data`, `plan`, `compare`, `summary`, `sensitivity`",
      },
    ],
    code: `library(weasel)

res <- weasel_example(seed = 42)

# the returned list contains:
# res$data    : the 1,000-respondent dummy panel
# res$plan    : the weasel_plan() object
# res$compare : the scored scenario comparison table
# res$summary : weasel_summarize_subset() output for anchored_balanced
# res$sensitivity : weasel_sensitivity() tolerance sweep for the plan

weasel_print_table(res$compare, title = "Scenarios from the demo")

balanced <- weasel_apply(res$plan, "anchored_balanced")
dim(balanced)`,
    note:
      "`weasel_example()` runs on the generator's default output, which contains genuine " +
      "wave-level missingness, so the Scope step prints a varied pattern table and the three " +
      "scenarios retain different samples. Explore `res$plan$id_metrics` for per-respondent " +
      "gap and missingness data, `res$summary$per_wave_coverage` for wave-level participation " +
      "counts, and `res$sensitivity` for the tolerance sweep.",
  },
};

// url state: every node is reachable by exactly one path from the root,
// so a readable slug for the current node encodes the entire flow

const SLUGS = {
  goal: "goal",
  dummy_purpose: "dummy-data",
  dummy_goal: "dummy-pipeline",
  scope_range: "scope-range",
  plan_criteria: "plan-criteria",
  plan_doc: "plan-methods",
  plan_span_justify: "plan-methods-span",
  plan_span: "plan-span",
  result_scope_full: "scope-full",
  result_scope_bounded: "scope-bounded",
  result_scope_learn: "scope-learn",
  result_plan_custom: "plan-custom",
  result_plan_core: "plan-core",
  result_plan_full: "plan-full",
  result_plan_justify_core: "plan-core-methods",
  result_plan_justify_full: "plan-full-methods",
  result_plan_learn: "plan-learn",
  result_demo: "demo",
};

const SLUG_TO_NODE = {};
for (const id in SLUGS) SLUG_TO_NODE[SLUGS[id]] = id;

// child -> { nodeId: parent, answer } lookup, built once from the tree
const PARENT = {};
for (const id in TREE) {
  const node = TREE[id];
  if (node.type !== "question") continue;
  PARENT[node.yes.next] = { nodeId: id, answer: "yes" };
  PARENT[node.no.next] = { nodeId: id, answer: "no" };
}

// rebuild the answer trail leading from the root to a node
function pathTo(nodeId) {
  const trail = [];
  let cursor = nodeId;
  while (PARENT[cursor]) {
    trail.unshift(PARENT[cursor]);
    cursor = PARENT[cursor].nodeId;
  }
  return trail;
}

// state

let state = {
  history: [], // { nodeId, answer: 'yes'|'no' }
  current: "start",
};

const BASE_TITLE = document.title;

// helpers

function esc(str) {
  return String(str)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

// inline code: escape, then turn `code` spans into <code> (prose fields only)
function ic(str) {
  return esc(str).replace(
    /`([^`]+)`/g,
    (m, p1) => `<code class="ic">${p1}</code>`,
  );
}

// minimal R syntax highlighter (presentational only; copy still reads textContent)
function highlightR(src) {
  const KW = new Set([
    "library", "require", "requireNamespace", "for", "while", "repeat", "if",
    "else", "function", "return", "break", "next", "in", "TRUE", "FALSE",
    "NULL", "NA", "NA_integer_", "NA_real_", "NA_character_", "Inf", "NaN",
  ]);
  const re =
    /(#[^\n]*)|("(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')|(\b\d+(?:\.\d+)?[Lf]?\b)|([A-Za-z.][A-Za-z0-9._]*)/g;
  let out = "";
  let last = 0;
  let m;
  while ((m = re.exec(src)) !== null) {
    out += esc(src.slice(last, m.index));
    const tok = m[0];
    if (m[1] != null) {
      out += `<span class="r-co">${esc(tok)}</span>`;
    } else if (m[2] != null) {
      out += `<span class="r-st">${esc(tok)}</span>`;
    } else if (m[3] != null) {
      out += `<span class="r-nu">${esc(tok)}</span>`;
    } else {
      const after = src.slice(m.index + tok.length);
      if (KW.has(tok)) out += `<span class="r-kw">${esc(tok)}</span>`;
      else if (/^\s*\(/.test(after)) out += `<span class="r-fn">${esc(tok)}</span>`;
      else out += esc(tok);
    }
    last = m.index + tok.length;
  }
  out += esc(src.slice(last));
  return out;
}

// url sync

function currentSlug() {
  return SLUGS[state.current] || "";
}

function flowUrl() {
  const slug = currentSlug();
  return location.href.split("#")[0] + (slug ? "#" + slug : "");
}

// reflect the current node in the address bar; one history entry per answer
function syncUrl() {
  const slug = currentSlug();
  if (location.hash.replace(/^#/, "") === slug) return;
  try {
    history.pushState(
      null,
      "",
      slug ? "#" + slug : location.pathname + location.search,
    );
  } catch (e) {
    location.hash = slug;
  }
}

function stateFromUrl() {
  let slug = location.hash.replace(/^#/, "");
  try {
    slug = decodeURIComponent(slug);
  } catch (e) {
    // malformed escape sequence: treat as unknown
  }
  const nodeId = SLUG_TO_NODE[slug];
  if (!nodeId) return { history: [], current: "start" };
  return { history: pathTo(nodeId), current: nodeId };
}

// re-derive state from the url (back/forward buttons, manual hash edits)
function applyUrl() {
  const next = stateFromUrl();
  if (next.current === state.current) return;
  state = next;
  render();
  scrollToStage();
}

// rendering

function render() {
  renderTrail();
  renderStage();
  syncTitle();
}

function syncTitle() {
  const node = TREE[state.current];
  document.title =
    node.type === "result" ? node.heading + " · weasel" : BASE_TITLE;
}

function scrollToStage(behavior) {
  document
    .getElementById("tree-stage")
    .scrollIntoView({ behavior: behavior || "smooth", block: "start" });
}

function renderTrail() {
  const trail = document.getElementById("path-trail");
  if (state.history.length === 0) {
    trail.innerHTML = "";
    return;
  }

  const items = state.history
    .map((item, i) => {
      const node = TREE[item.nodeId];
      const tag = item.answer === "yes" ? "Yes" : "No";
      return `<li class="trail-item">
      <button class="trail-link" onclick="goBackTo(${i})"
        title="Change this answer"
        aria-label="Go back to: ${esc(node.shortLabel)}">
        <span class="trail-topic">${esc(node.shortLabel)}</span>
        <span class="trail-tag trail-tag--${item.answer}">${tag}</span>
      </button>
    </li>`;
    })
    .join("");

  trail.innerHTML = `
    <ol class="trail-items">${items}</ol>
    <button class="trail-restart" onclick="restart()" aria-label="Start over">&#8634; Start over</button>`;
}

function renderStage() {
  const stage = document.getElementById("tree-stage");
  const node = TREE[state.current];
  stage.innerHTML =
    node.type === "question"
      ? buildQuestion(node, state.current)
      : buildResult(node);

  // re-trigger entrance animation
  const card = stage.querySelector(".node-card");
  if (card) {
    card.style.animation = "none";
    void card.offsetHeight;
    card.style.animation = "";
  }
}

function buildQuestion(node, nodeId) {
  const backBtn =
    state.history.length > 0
      ? `<button class="btn-back" onclick="goBack()" aria-label="Go back one step">&#8592; Back</button>`
      : "";
  return `
    <div class="node-card">
      <div class="q-topbar">
        <span class="q-step-label">Step ${state.history.length + 1}</span>
        ${backBtn}
      </div>
      <h2 class="q-question">${esc(node.question)}</h2>
      <p class="q-explanation">${ic(node.explanation)}</p>
      <div class="yn-group" role="group" aria-label="Yes or No">
        <button class="yn-btn yn-yes" onclick="choose('${nodeId}','yes')" aria-label="${esc(node.yes.label)}">
          <span class="yn-marker">Yes</span>
          <span class="yn-text">${esc(node.yes.label)}</span>
        </button>
        <button class="yn-btn yn-no" onclick="choose('${nodeId}','no')" aria-label="${esc(node.no.label)}">
          <span class="yn-marker">No</span>
          <span class="yn-text">${esc(node.no.label)}</span>
        </button>
      </div>
    </div>`;
}

function buildResult(node) {
  const steps = node.steps
    .map(
      (s, i) => `
    <div class="step-row">
      <span class="step-num" aria-hidden="true">${i + 1}</span>
      <div class="step-body">
        <code class="step-fn">${esc(s.fn)}</code>
        <span class="step-note">${ic(s.note)}</span>
      </div>
    </div>`,
    )
    .join("");

  const noteHtml = node.note
    ? `
    <div class="result-note" role="note">
      <span class="note-icon" aria-hidden="true">&#9432;</span>
      <span>${ic(node.note)}</span>
    </div>`
    : "";

  const isDemo = node.pipeline === "Full Demonstration";

  return `
    <div class="node-card result-card${isDemo ? " result-demo-card" : ""}">
      <span class="result-badge ${node.badgeClass}">${esc(node.pipeline)}</span>
      <h2 class="result-heading">${esc(node.heading)}</h2>
      <p class="result-summary">${ic(node.summary)}</p>

      <span class="workflow-label">Workflow steps</span>
      <div class="workflow-steps">${steps}</div>

      <div class="code-wrapper">
        <div class="code-header">
          <span class="code-lang">R</span>
          <button class="code-copy-btn" onclick="copyCode(this)" aria-label="Copy R code">Copy</button>
        </div>
        <pre class="code-block"><code>${highlightR(node.code)}</code></pre>
      </div>

      ${noteHtml}
      <div class="result-actions">
        <button class="btn-share" onclick="copyFlowLink(this)">Copy link to this recipe</button>
        <button class="btn-back" onclick="goBack()" aria-label="Go back one step">&#8592; Back</button>
        <button class="btn-restart" onclick="restart()">&#8634; Start over</button>
      </div>
    </div>`;
}

// interactions

function choose(nodeId, answer) {
  const node = TREE[nodeId];
  state.history.push({ nodeId, answer });
  state.current = answer === "yes" ? node.yes.next : node.no.next;
  syncUrl();
  render();
  setTimeout(() => scrollToStage(), 50);
}

function restart() {
  state = { history: [], current: "start" };
  syncUrl();
  render();
  setTimeout(() => scrollToStage(), 50);
}

// undo the most recent answer and re-ask that question
function goBack() {
  goBackTo(state.history.length - 1);
}

// rewind to the question at trail position i (0-based); the answers
// after it are discarded so the path can be rebuilt from there
function goBackTo(i) {
  if (i < 0 || i >= state.history.length) return;
  const target = state.history[i].nodeId;
  state.history = state.history.slice(0, i);
  state.current = target;
  syncUrl();
  render();
  setTimeout(() => scrollToStage(), 50);
}

function copyFlowLink(btn) {
  if (btn.classList.contains("copied")) return;
  const url = flowUrl();
  const label = btn.textContent;
  const done = () => {
    btn.textContent = "Link copied";
    btn.classList.add("copied");
    setTimeout(() => {
      btn.textContent = label;
      btn.classList.remove("copied");
    }, 1800);
  };
  navigator.clipboard
    .writeText(url)
    .then(done)
    .catch(() => {
      // clipboard api unavailable: fall back to a hidden textarea
      const ta = document.createElement("textarea");
      ta.value = url;
      ta.setAttribute("readonly", "");
      ta.style.position = "absolute";
      ta.style.left = "-9999px";
      document.body.appendChild(ta);
      ta.select();
      try {
        document.execCommand("copy");
        done();
      } catch (e) {
        // leave the url selected so the user can copy manually
      }
      document.body.removeChild(ta);
    });
}

function copyCode(btn) {
  const codeEl = btn.closest(".node-card").querySelector(".code-block code");
  if (!codeEl) return;
  navigator.clipboard
    .writeText(codeEl.textContent)
    .then(() => {
      btn.textContent = "Copied";
      btn.classList.add("copied");
      setTimeout(() => {
        btn.textContent = "Copy";
        btn.classList.remove("copied");
      }, 1800);
    })
    .catch(() => {
      const range = document.createRange();
      range.selectNodeContents(codeEl);
      window.getSelection().removeAllRanges();
      window.getSelection().addRange(range);
    });
}

// appearance follows the system until a user explicitly selects a theme
function initTheme() {
  const root = document.documentElement;
  const toggle = document.getElementById("theme-toggle");
  const systemTheme = window.matchMedia("(prefers-color-scheme: dark)");

  function updateToggle() {
    const dark = root.dataset.theme === "dark" ||
      (!root.dataset.theme && systemTheme.matches);
    toggle.setAttribute("aria-pressed", String(dark));
    toggle.title = dark ? "Switch to light mode" : "Switch to dark mode";
  }

  toggle.addEventListener("click", () => {
    const theme = toggle.getAttribute("aria-pressed") === "true" ? "light" : "dark";
    root.dataset.theme = theme;
    try {
      localStorage.setItem("weasel-theme", theme);
    } catch (e) {
      // manual switching still works when storage is unavailable
    }
    updateToggle();
  });

  systemTheme.addEventListener("change", updateToggle);
  window.addEventListener("storage", (event) => {
    if (event.key !== "weasel-theme" && event.key !== null) return;
    try {
      if (event.storageArea !== localStorage) return;
    } catch (e) {
      // no shared preference can be read when storage is unavailable
      return;
    }
    if (event.newValue === "light" || event.newValue === "dark") {
      root.dataset.theme = event.newValue;
    } else {
      delete root.dataset.theme;
    }
    updateToggle();
  });

  updateToggle();
  toggle.hidden = false;
}

// init

document.addEventListener("DOMContentLoaded", () => {
  initTheme();
  const rawHash = location.hash.replace(/^#/, "");
  state = stateFromUrl();
  if (rawHash && state.current === "start") {
    // unknown slug: drop it so the address bar matches the fresh start
    try {
      history.replaceState(null, "", location.pathname + location.search);
    } catch (e) {
      // ignore; the stale hash is harmless
    }
  }
  render();
  if (state.current !== "start") {
    // jump straight to the shared step without animated scrolling
    const root = document.documentElement;
    root.style.scrollBehavior = "auto";
    scrollToStage("auto");
    root.style.scrollBehavior = "";
  }

  // back/forward buttons and manual hash edits re-derive state from
  // the url; registered here so the file stays free of load-time
  // browser globals (the guide-snippets CI loads it with a bare
  // document stub to read TREE)
  window.addEventListener("popstate", applyUrl);
  window.addEventListener("hashchange", applyUrl);
});
