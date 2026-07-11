/* weasel decision guide: tree data + ui controller */

const TREE = {
  start: {
    type: "question",
    shortLabel: "Own data",
    question:
      "Do you have your own longitudinal panel data ready to work with?",
    explanation:
      "Both pipelines expect a long-format data frame (one row per respondent per wave) " +
      "with a respondent identifier column and a wave or time column. " +
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
      "The Scope pipeline maps which respondents were observed at which waves and groups " +
      "them into participation fingerprints. This is useful when you do not yet have a " +
      "clear selection rule in mind and want to understand the missingness landscape first. " +
      "If you already know the structural criteria you want to apply (required endpoints, " +
      "a missingness tolerance, a gap limit), go directly to the Plan pipeline, which is " +
      "the pipeline that actually enforces those constraints.",
    yes: { label: "Yes, I want to map patterns first", next: "scope_range" },
    no: {
      label: "No, I want to construct a sample directly",
      next: "plan_doc",
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

  plan_doc: {
    type: "question",
    shortLabel: "Methods documentation",
    question:
      "Do you need to formally document the selection in a methods section or pre-registration?",
    explanation:
      "`weasel_justify_subset()` generates a structured paragraph (in methods, concise, or " +
      "extended style) that documents the window bounds, structural constraints, resulting " +
      "sample size, and key coverage statistics, with optional in-text citation. " +
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
    shortLabel: "Fixed wave count",
    question:
      "Does your model require a fixed number of consecutive waves, for example a growth-curve or latent-trajectory model?",
    explanation:
      "`span = \"core\"` asks weasel to locate the highest-coverage consecutive window of " +
      "a specified length (`core_len`, default 6). This ensures every respondent in the " +
      "analysis is observed within the same bounded segment of the panel. " +
      "`span = \"full\"` includes every observed wave and is appropriate when the wave count " +
      "need not be fixed, for example in random-intercept models or survival analyses " +
      "where respondents can have different observation lengths.",
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
    shortLabel: "Fixed wave count",
    question: "Does your model require a fixed number of consecutive waves?",
    explanation:
      "`span = \"core\"` selects the densest consecutive window of fixed length. " +
      "This is typically the right choice for growth-curve models or any design " +
      "where a uniform set of L waves per respondent is analytically important. " +
      "`span = \"full\"` covers all observed waves and gives you the widest longitudinal " +
      "window at the cost of greater within-window missingness variability.",
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
    summary:
      "Explore all waves without restriction. The presence matrix covers the complete " +
      "observed range, giving an unfiltered view of participation patterns across every " +
      "wave in your panel. This is the appropriate starting point when you have no " +
      "prior knowledge of where missingness is concentrated.",
    steps: [
      {
        fn: 'set_weasel_scope(data, "id", "time")',
        note: "initialise the scope environment; no bounds means weasel uses the full observed range",
      },
      {
        fn: "evaluate_weasel_scope()",
        note: "resolve lower and upper bounds from the data; inspect them before pivoting if needed",
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

set_weasel_scope(d, "id", "time")
evaluate_weasel_scope()
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
    summary:
      "Focus on a specific wave window. Bounding the range with `upper` (and optionally `lower`) " +
      "keeps the pattern table interpretable and removes sparse early or late waves. " +
      "Respondents who skip waves within the window are retained; their gaps show up as " +
      "distinct fingerprints (the '.' marks) rather than being filtered out.",
    steps: [
      {
        fn: 'set_weasel_scope(data, "id", "time", upper = N)',
        note: "restrict to waves 1 through N; add `lower = M` for a non-zero start",
      },
      {
        fn: "evaluate_weasel_scope()",
        note: "finalise and validate the declared bounds",
      },
      { fn: "weasel_reshape_to_wide()", note: "build the restricted presence matrix" },
      {
        fn: "weasel_summarize_waves()",
        note: "enumerate distinct participation patterns within the window",
      },
      {
        fn: "weasel_filter_wave_summary(ids_range = c(5, Inf))",
        note: "optional: show only patterns shared by at least 5 respondents",
      },
      {
        fn: "weasel_get_data_by_row(i)",
        note: "retrieve long-format data for the selected pattern row",
      },
    ],
    code: `library(weasel)

d <- your_data

set_weasel_scope(d, "id", "time", upper = 10)
evaluate_weasel_scope()
weasel_reshape_to_wide()
weasel_summarize_waves()

weasel_print_table(
  weasel_filter_wave_summary(ids_range = c(5, Inf)),
  title = "Common patterns (n >= 5)"
)

subset1 <- weasel_get_data_by_row(1)`,
    note:
      "`max_gap_len` and `n_gap_max` are enforced at reshape time inside the bounded window: " +
      "interior gaps are measured strictly between a respondent's first and last observed " +
      "wave within the span, so late entry and early exit never count as gaps. Respondents " +
      "exceeding the limits are dropped with a status message. The same constraint names are " +
      "used by plan scenario tables; to weigh several tolerances against each other before " +
      "committing, use the Plan pipeline (`weasel_plan()` scenarios or " +
      "`weasel_sensitivity()`).",
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

  result_plan_core: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: core window",
    summary:
      "weasel locates the highest-coverage consecutive wave window of length `core_len` " +
      "(default 6), then evaluates three built-in scenarios: `anchored_strict` (no missing " +
      "waves, both endpoints required), `anchored_balanced` (at most 1 missing, both endpoints), " +
      "and `lenient_info_max` (at most 2 missing, endpoints not required), scoring each on " +
      "coverage, endpoint rate, and gap structure. Review the scored table and choose the " +
      "scenario that best fits your analytical requirements.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "core")',
        note: "build and score three scenarios over the highest-coverage consecutive window; returns a plan object",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "compute composite scores; the highest-scoring scenario is flagged as `recommended`",
      },
      {
        fn: 'weasel_summarize_subset(p, scenario, data, "id", "time")',
        note: "audit per-wave coverage and missingness distribution before extracting data",
      },
      {
        fn: "weasel_apply(p, scenario)",
        note: "produce the filtered long-format data frame for the chosen scenario",
      },
    ],
    code: `library(weasel)

d <- your_data

p   <- weasel_plan(d, "id", "time", span = "core")
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenario comparison")
cat(weasel_compare_to_sentence(cmp), "\\n")

rec <- cmp$scenario[cmp$recommended]
s   <- weasel_summarize_subset(p, rec, d, "id", "time")
weasel_print_table(s$headline)
weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")

analysis_data <- weasel_apply(p, rec)
dim(analysis_data)`,
    note: null,
  },

  result_plan_full: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: full span",
    summary:
      "All observed waves are included in the evaluation window. Scenarios are scored " +
      "against the complete panel range. Use this when your model does not require a " +
      "fixed wave count, or when retaining the widest possible observation window " +
      "matters more than within-window completeness.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "full")',
        note: "evaluate all three default scenarios across every observed wave",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "score and rank scenarios; inspect the `recommended` flag",
      },
      {
        fn: 'weasel_summarize_subset(p, scenario, data, "id", "time")',
        note: "verify per-wave coverage and missingness statistics for the chosen scenario",
      },
      {
        fn: "weasel_apply(p, scenario)",
        note: "extract the analysis-ready long-format data frame",
      },
    ],
    code: `library(weasel)

d <- your_data

p   <- weasel_plan(d, "id", "time", span = "full")
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Full-span scenarios")
cat(weasel_compare_to_sentence(cmp), "\\n")

s <- weasel_summarize_subset(p, "anchored_balanced", d, "id", "time")
cat(weasel_subset_to_sentence(s), "\\n")

analysis_data <- weasel_apply(p, "anchored_balanced")
dim(analysis_data)`,
    note: null,
  },

  result_plan_justify_core: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: core window with methods justification",
    summary:
      "Build a scenario plan over the highest-coverage consecutive wave window, " +
      "select a scenario, and generate a structured paragraph for your methods section. " +
      "`weasel_justify_subset()` produces text in three verbosity styles and supports " +
      "formal in-text citation. Designed for pre-registered studies and manuscripts " +
      "where the data-preparation stage must be documented transparently.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "core")',
        note: "score scenarios over the best-coverage consecutive window; window bounds are stored in the plan object",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "compute composite scores; use the `recommended` flag as a starting point, not a final decision",
      },
      {
        fn: 'weasel_summarize_subset(p, scenario, data, "id", "time")',
        note: "inspect headline statistics, per-wave coverage, and missingness distribution before committing",
      },
      {
        fn: "weasel_apply(p, scenario)",
        note: "extract the filtered long-format data frame",
      },
      {
        fn: "weasel_justify_subset(p, scenario)",
        note: 'generate the methods paragraph; `style` controls verbosity: `"methods"` (default), `"concise"`, or `"extended"`',
      },
    ],
    code: `library(weasel)

d <- your_data

p   <- weasel_plan(d, "id", "time", span = "core")
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Scenario comparison")

s <- weasel_summarize_subset(p, "anchored_balanced", d, "id", "time")
weasel_print_table(s$headline)

analysis_data <- weasel_apply(p, "anchored_balanced")

# full methods-section paragraph
cat(weasel_justify_subset(p, "anchored_balanced"), "\\n")

# two-sentence version for supplementary material
cat(weasel_justify_subset(p, "anchored_balanced", style = "concise"), "\\n")

# with formal in-text citation
cat(weasel_justify_subset(p, "anchored_balanced",
                          author = "Last Name", year = "2026"), "\\n")`,
    note:
      'Three verbosity styles are available: `"methods"` (full paragraph, default), ' +
      '`"concise"` (two sentences, suitable for supplementary sections), and ' +
      '`"extended"` (detailed rationale with sensitivity framing). ' +
      "Supply `author` and `year` to embed a formal citation instead of the default package reference.",
  },

  result_plan_justify_full: {
    type: "result",
    pipeline: "Plan Pipeline",
    badgeClass: "badge-plan",
    heading: "Plan pipeline: full span with methods justification",
    summary:
      "Evaluate scenarios over all available waves and produce a fully documented, " +
      "reproducible selection. The paragraph from `weasel_justify_subset()` will describe " +
      "the full observed wave range, the structural constraints applied, and the resulting " +
      "sample-size and coverage statistics.",
    steps: [
      {
        fn: 'weasel_plan(data, "id", "time", span = "full")',
        note: "score all three scenarios over the complete wave range",
      },
      {
        fn: "weasel_compare_scenarios(p)",
        note: "display and inspect the scored scenario table",
      },
      {
        fn: 'weasel_summarize_subset(p, scenario, data, "id", "time")',
        note: "verify coverage statistics for the chosen scenario",
      },
      {
        fn: "weasel_apply(p, scenario)",
        note: "produce the analysis-ready data frame",
      },
      {
        fn: "weasel_justify_subset(p, scenario)",
        note: "generate the methods paragraph documenting window bounds, constraints, and statistics",
      },
    ],
    code: `library(weasel)

d <- your_data

p   <- weasel_plan(d, "id", "time", span = "full")
cmp <- weasel_compare_scenarios(p)
weasel_print_table(cmp, title = "Full-span scenarios")

analysis_data <- weasel_apply(p, "anchored_balanced")

cat(weasel_justify_subset(p, "anchored_balanced",
                          author = "Last Name", year = "2026"), "\\n")`,
    note:
      "Supply `author` and `year` for a formal citation. Style options: " +
      '`"methods"` (default), `"concise"`, and `"extended"`.',
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
        fn: 'weasel_summarize_subset(p, "anchored_balanced", d, "id", "time")',
        note: "audit headline statistics, per-wave coverage, and the missingness distribution",
      },
      {
        fn: 'weasel_apply(p, "anchored_balanced")',
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

s <- weasel_summarize_subset(p, "anchored_balanced", d, "id", "time")
weasel_print_table(s$headline, title = "Subset headline")
weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")
cat(weasel_subset_to_sentence(s), "\\n")

balanced <- weasel_apply(p, "anchored_balanced")
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

// state

let state = {
  history: [], // { nodeId, answer: 'yes'|'no' }
  current: "start",
};

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

// rendering

function render() {
  renderTrail();
  renderStage();
}

function renderTrail() {
  const trail = document.getElementById("path-trail");
  if (state.history.length === 0) {
    trail.innerHTML = "";
    return;
  }

  const items = state.history
    .map((item) => {
      const node = TREE[item.nodeId];
      const tag = item.answer === "yes" ? "Yes" : "No";
      return `<li class="trail-item">
      <span class="trail-topic">${esc(node.shortLabel)}</span>
      <span class="trail-tag trail-tag--${item.answer}">${tag}</span>
    </li>`;
    })
    .join("");

  trail.innerHTML = `
    <ol class="trail-items">${items}</ol>
    <button class="trail-restart" onclick="restart()" aria-label="Start over">&#8592; Start over</button>`;
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
  return `
    <div class="node-card">
      <span class="q-step-label">Step ${state.history.length + 1}</span>
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
      <button class="btn-restart" onclick="restart()">&#8592; Start over</button>
    </div>`;
}

// interactions

function choose(nodeId, answer) {
  const node = TREE[nodeId];
  state.history.push({ nodeId, answer });
  state.current = answer === "yes" ? node.yes.next : node.no.next;
  render();
  setTimeout(() => {
    document
      .getElementById("tree-stage")
      .scrollIntoView({ behavior: "smooth", block: "start" });
  }, 50);
}

function restart() {
  state = { history: [], current: "start" };
  render();
  setTimeout(() => {
    document
      .getElementById("tree-stage")
      .scrollIntoView({ behavior: "smooth", block: "start" });
  }, 50);
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

// init

document.addEventListener("DOMContentLoaded", render);
