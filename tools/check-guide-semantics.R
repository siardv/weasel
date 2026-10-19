# guide semantic parity check
#
# the guide-snippets workflow proves every guide snippet *executes*;
# executability and semantic agreement are different guarantees, and
# this script also checks that displayed selections, diagnostics and
# prose describe the same sample, including targeted edge cases. run
# from the repository root with the package installed and Node.js
# available (CI runs it after the snippet step).

options(weasel.verbose = FALSE)
suppressMessages(library(weasel))

fail <- function(...) {
  cat("SEMANTIC CHECK FAIL:", ..., "\n")
  quit(status = 1L)
}

app <- readLines(file.path("docs", "app.js"), warn = FALSE)

# title greps: the bounded recipe filters on `ids` (respondents per
# pattern), so its displayed title must say ids >= 5; `n` means
# observed waves in the package's own vocabulary
if (!any(grepl("Common patterns (ids >= 5)", app, fixed = TRUE))) {
  fail("bounded-recipe title 'Common patterns (ids >= 5)' not found in docs/app.js")
}
if (any(grepl("Common patterns (n >= 5)", app, fixed = TRUE))) {
  fail("bounded-recipe title mislabels the ids filter as an n filter")
}

# the recipe must extract through the stable pattern id of the named
# filtered table, guarded against the empty-filter case
if (!any(grepl("weasel_get_data_by_row(common$pattern[1])", app, fixed = TRUE))) {
  fail("bounded recipe does not extract via common$pattern[1]")
}
if (!any(grepl("nrow(common) > 0", app, fixed = TRUE))) {
  fail("bounded recipe does not guard the empty filtered table")
}

# execute the recipe's semantics with the CI panel and assert parity:
# the participation string of every extracted respondent must equal
# the first displayed filtered pattern
your_data <- generate_weasel_dummy_data(n_ids = 120, n_times = 10, seed = 99)
set_weasel_scope(your_data, "id", "time", upper = 10)
evaluate_weasel_scope()
weasel_reshape_to_wide()
weasel_summarize_waves()
common <- weasel_filter_wave_summary(ids_range = c(5, Inf))
if (nrow(common) == 0) {
  fail("CI panel produced an empty filtered table; parity cannot be checked")
}
extracted <- weasel_get_data_by_row(common$pattern[1])

span <- 1:10
pattern_of <- function(one_id) {
  w <- sort(unique(extracted$time[extracted$id == one_id]))
  w <- w[w %in% span]
  paste(ifelse(span %in% w, as.character(span), "."), collapse = " ")
}
ids <- unique(extracted$id)
patterns <- vapply(ids, pattern_of, character(1))
if (!all(patterns == common$waves[1])) {
  fail("extracted respondents do not all match the displayed pattern '",
       common$waves[1], "'")
}
weasel_clear_scope()

cat("guide semantics OK: displayed pattern equals extracted pattern;",
    "titles label the filtered column correctly\n")

assert <- function(ok, ...) {
  if (!isTRUE(ok)) fail(...)
}

# extract the actual template literals through JavaScript, as the
# snippet workflow does; no text parser or extra R dependency is needed
snippet_dir <- tempfile("weasel_guide_semantics_")
dir.create(snippet_dir)
node <- Sys.which("node")
assert(nzchar(node), "Node.js is required to extract the guide recipes")
extract_script <- paste(
  'const fs = require("fs"), vm = require("vm");',
  'const src = fs.readFileSync("docs/app.js", "utf8");',
  'vm.runInNewContext(src + "\\n;__out(TREE);", {',
  '  document: { addEventListener() {} },',
  '  __out(tree) {',
  '    for (const [key, node] of Object.entries(tree)) {',
  '      if (node && node.code) {',
  '        fs.writeFileSync(process.argv[1] + "/" + key + ".R", node.code);',
  '      }',
  '    }',
  '  }',
  '});',
  sep = "\n"
)
status <- system2(node, c("-e", shQuote(extract_script), shQuote(snippet_dir)))
assert(identical(status, 0L), "could not extract guide snippets")
assert(length(list.files(snippet_dir, pattern = "\\.R$")) == 10L,
       "expected ten guide snippets")

# run the copied recipe, recording calls and returned tables without
# replacing package behavior. only explicit reader choices are changed
# when exercising a different grid, covariate or named scenario
run_recipe <- function(recipe, data, diagnostic_vars = NULL,
                       wave_grid = NULL, chosen_scenario = NULL) {
  expressions <- parse(file.path(snippet_dir, paste0(recipe, ".R")))
  choices <- list(diagnostic_vars = diagnostic_vars, wave_grid = wave_grid,
                  chosen_scenario = chosen_scenario)
  for (choice in names(choices)) {
    if (is.null(choices[[choice]])) next
    assignment <- vapply(expressions, function(x) {
      is.call(x) && identical(x[[1]], as.name("<-")) &&
        identical(x[[2]], as.name(choice))
    }, logical(1))
    assert(sum(assignment) == 1L,
           recipe, "must have one explicit", choice, "assignment")
    expressions[[which(assignment)]][[3]] <- choices[[choice]]
  }
  env <- new.env(parent = globalenv())
  env$your_data <- data
  trace <- new.env(parent = emptyenv())
  trace$calls <- list()
  for (api_name in c("weasel_summarize_subset", "weasel_apply",
                     "weasel_justify_subset", "weasel_selectivity",
                     "weasel_print_table", "weasel_plan", "set_weasel_scope",
                     "evaluate_weasel_scope", "weasel_scope_info",
                     "weasel_reshape_to_wide", "weasel_get_data_by_row")) {
    env[[api_name]] <- local({
      name <- api_name
      function(...) {
        args <- list(...)
        index <- length(trace$calls) + 1L
        trace$calls[[index]] <- list(name = name, args = args)
        result <- do.call(getExportedValue("weasel", name), args)
        trace$calls[[index]]$result <- result
        result
      }
    })
  }
  env$print <- function(...) {
    args <- list(...)
    trace$calls[[length(trace$calls) + 1L]] <- list(name = "print", args = args)
    base::print(...)
  }
  error <- NULL
  warnings <- list()
  output <- capture.output(withCallingHandlers(
    tryCatch(eval(expressions, envir = env), error = function(e) error <<- e),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  ))
  list(env = env, calls = trace$calls, error = error, warnings = warnings,
       output = output)
}

calls_named <- function(run, name) {
  Filter(function(x) identical(x$name, name), run$calls)
}
selection_calls <- function(run) {
  Filter(function(x) x$name %in% c("weasel_summarize_subset", "weasel_apply",
    "weasel_justify_subset", "weasel_selectivity"), run$calls)
}

# the balanced scenario wins here, but strict retains only two people:
# a hard-coded strict diagnostic would compare a different sample
panel <- do.call(rbind, lapply(1:25, function(i) {
  times <- if (i <= 2) 1:6 else if (i <= 20) c(1, 2, 4, 5, 6) else 2:5
  data.frame(id = i, time = times, x = 10 * i + times)
}))
plan_recipes <- c("result_plan_core", "result_plan_full",
                  "result_plan_justify_core", "result_plan_justify_full")
for (recipe in plan_recipes) {
  run <- run_recipe(recipe, panel)
  assert(is.null(run$error), recipe, "failed on the balanced fixture")
  assert(identical(run$env$chosen_scenario, "anchored_balanced"),
         recipe, "did not choose the nonempty recommended scenario")
  assert(setequal(unique(run$env$analysis_data$id), 1:20),
         recipe, "extracted the wrong respondents")
  assert(all(vapply(selection_calls(run), function(x) {
    identical(x$args[[2]], run$env$chosen_scenario)
  }, logical(1))), recipe, "used inconsistent scenarios")
}
core <- run_recipe("result_plan_core", panel, diagnostic_vars = "x")
assert(is.null(core$error), "core recipe rejected an eligible covariate")
diagnostics <- calls_named(core, "weasel_selectivity")
assert(length(diagnostics) == 1L, "core recipe did not run the opted-in diagnostic")
diagnostic <- diagnostics[[1]]
assert(identical(diagnostic$args[[2]], core$env$chosen_scenario),
       "diagnostic used a different scenario from extraction")
assert(identical(diagnostic$result$n_retained, 20L) &&
         identical(diagnostic$result$n_excluded, 5L),
       "diagnostic must compare the 20 selected and 5 excluded respondents")
assert(identical(core$env$s$headline$scenario, core$env$chosen_scenario) &&
         core$env$s$headline$n_ids == 20L,
       "summary does not describe the extracted scenario")
covariates <- panel
covariates$flag <- covariates$id %% 2L == 0L
covariates$sparse_x <- ifelse(covariates$id %in% 2:20, NA_real_, covariates$x)
covariates$constant <- 1
covariate_run <- run_recipe("result_plan_core", covariates,
  diagnostic_vars = c("flag", "sparse_x", "constant"))
assert(is.null(covariate_run$error), "core recipe rejected eligible covariates")
covariate_result <- calls_named(covariate_run, "weasel_selectivity")[[1]]$result
logical_result <- covariate_result[covariate_result$variable == "flag", ]
sparse_result <- covariate_result[covariate_result$variable == "sparse_x", ]
constant_result <- covariate_result[covariate_result$variable == "constant", ]
assert(logical_result$n_retained == 20L && logical_result$n_excluded == 5L &&
         is.finite(logical_result$smd), "logical covariate diagnostic is incomplete")
assert(sparse_result$n_retained == 1L && sparse_result$n_excluded == 5L &&
         is.na(sparse_result$smd) && is.na(constant_result$smd),
       "per-covariate missingness/variance prerequisites are not represented")
invalid_covariates <- panel
invalid_covariates$label <- as.character(invalid_covariates$id)
for (variable in c("not_in_data", "label", "id", "time")) {
  invalid <- run_recipe("result_plan_core", invalid_covariates,
                        diagnostic_vars = variable)
  assert(inherits(invalid$error, "error") &&
           grepl("Choose numeric/logical covariates", conditionMessage(invalid$error),
                 fixed = TRUE) &&
           length(calls_named(invalid, "weasel_selectivity")) == 0L,
         "core recipe did not reject ineligible diagnostic variable:", variable)
}
observed <- table(table(core$env$analysis_data$id))
assert(identical(as.integer(observed[c("5", "6")]), c(18L, 2L)),
       "balanced fixed-window fixture must include incomplete observations")
strict <- weasel_apply(core$env$p, "anchored_strict")
assert(length(unique(strict$id)) == 2L && all(table(strict$id) == 6L),
       "strict fixture does not demonstrate complete participation")

# the displayed sensitivity table must contain all three tolerances,
# holding the other settings fixed for an interpretable one-factor sweep
for (recipe in c("result_plan_core", "result_plan_full")) {
  run <- run_recipe(recipe, panel)
  sweeps <- Filter(function(x) {
    is.data.frame(x$result) && "max_missing" %in% names(x$result) &&
      "prop_ids" %in% names(x$result)
  }, calls_named(run, "weasel_print_table"))
  assert(length(sweeps) == 1L, recipe, "did not display one sensitivity table")
  displayed <- sweeps[[1]]$result
  assert(nrow(displayed) == 3L &&
           identical(as.integer(displayed$max_missing), 0:2) &&
           length(unique(displayed$require_endpoints)) == 1L &&
           length(unique(displayed$n_gap_max)) == 1L &&
           length(unique(displayed$max_gap_len)) == 1L,
         recipe, "truncated or confounded the missing-wave sweep")
  assert(identical(as.integer(displayed$n_ids), c(2L, 20L, 20L)),
         recipe, "sensitivity display lost the fixture's tolerance effect")
}

# no default scenario retains anyone; fail deliberately before any
# summary, extraction or methods paragraph can imply a selected sample
sparse <- rbind(expand.grid(id = 1:5, time = 1:3),
                expand.grid(id = 6:10, time = 4:6))
for (recipe in plan_recipes) {
  run <- run_recipe(recipe, sparse)
  assert(inherits(run$error, "error"), recipe, "did not stop an empty recommendation")
  assert(grepl("recommend", conditionMessage(run$error), ignore.case = TRUE) &&
           !grepl("scenario not found", conditionMessage(run$error), fixed = TRUE),
         recipe, "did not explain the absent recommendation")
  assert(length(selection_calls(run)) == 0L,
         recipe, "called selection APIs before resolving an empty recommendation")
  assert(!exists("analysis_data", envir = run$env, inherits = FALSE),
         recipe, "created analysis data without a chosen scenario")
}

# an id/time-only panel still supports selection; covariate diagnostics
# are optional, and all-retained data have no excluded group to compare
no_covariates <- run_recipe("result_plan_core", panel[c("id", "time")])
assert(is.null(no_covariates$error) &&
         length(calls_named(no_covariates, "weasel_selectivity")) == 0L &&
         setequal(unique(no_covariates$env$analysis_data$id), 1:20),
       "id/time-only core recipe did not skip optional selectivity safely")
complete <- expand.grid(id = 1:5, time = 1:6)
complete$x <- complete$id
all_retained <- run_recipe("result_plan_core", complete, diagnostic_vars = "x")
assert(is.null(all_retained$error) &&
         length(calls_named(all_retained, "weasel_selectivity")) == 0L &&
         setequal(unique(all_retained$env$analysis_data$id), 1:5),
       "all-retained core recipe did not skip selectivity safely")
all_retained_error <- tryCatch(
  weasel_selectivity(all_retained$env$p, all_retained$env$chosen_scenario,
                    vars = "x"), error = identity)
assert(inherits(all_retained_error, "error") &&
         grepl("no excluded group", conditionMessage(all_retained_error), fixed = TRUE),
       "all-retained prerequisite claim does not match the actual API")

# min_present is a Scope constraint. for a fixed grid, Plan can express
# the same observation threshold with L - required observations. the
# observed-grid case prevents confusing L with the numeric bounds width
for (grid in c("consecutive", "observed")) {
  data <- panel
  if (grid == "observed") data$time <- 2000L + 2L * (data$time - 1L)
  for (required in c(3L, 5L, 6L)) {
    scenarios <- data.frame(
      scenario = "minimum_observations", require_endpoints = FALSE,
      max_missing = 6L - required, n_gap_max = Inf, max_gap_len = Inf
    )
    plan <- weasel_plan(data, "id", "time", span = "full", grid = grid,
                        scenarios = scenarios)
    L <- length(plan$span)
    assert(L == 6L && scenarios$max_missing == L - required,
           "observation equivalence fixture has the wrong grid length")
    set_weasel_scope(data, "id", "time", grid = grid,
                     lower = plan$lower, upper = plan$upper,
                     min_present = required)
    wide <- weasel_reshape_to_wide()
    selected <- weasel_apply(plan, "minimum_observations")
    assert(setequal(wide$id, selected$id), grid, "Scope/Plan threshold mismatch")
    weasel_clear_scope()
  }
}
ignored_minimum <- data.frame(
  scenario = "unsupported_minimum", require_endpoints = FALSE,
  min_present = 6L, max_missing = Inf, n_gap_max = Inf, max_gap_len = Inf
)
minimum_panel <- data.frame(id = c(rep("complete", 6), rep("partial", 3)),
                            time = c(1:6, 2:4))
ignored_plan <- weasel_plan(minimum_panel, "id", "time", span = "full",
                            scenarios = ignored_minimum)
assert(!("min_present" %in% names(ignored_plan$plan)) &&
         setequal(weasel_apply(ignored_plan, "unsupported_minimum")$id,
                  c("complete", "partial")),
       "Scope-only min_present claim no longer matches the Plan API")
supported_minimum <- ignored_minimum[setdiff(names(ignored_minimum), "min_present")]
supported_minimum$scenario <- "six_observations"
supported_minimum$max_missing <- 0L
supported_plan <- weasel_plan(minimum_panel, "id", "time", span = "full",
                              scenarios = supported_minimum)
assert(identical(unique(weasel_apply(supported_plan, "six_observations")$id),
                 "complete"),
       "supported zero-missing constraint retained a three-wave respondent")

# audit the actual recipe call and printed span, including their order:
# the declared choice must reach the API, and the resolved grid must be
# visible before selection. bounds alone cannot reveal an observed grid
assert_grid_audit <- function(run, recipe, grid, expected_span) {
  assert(is.null(run$error), recipe, "failed with grid", grid)
  plans <- calls_named(run, "weasel_plan")
  setups <- if (length(plans)) plans else calls_named(run, "set_weasel_scope")
  assert(length(setups) == 1L && identical(setups[[1]]$args$grid, grid),
         recipe, "did not pass its explicit grid choice to the API")
  if (length(plans)) {
    span <- plans[[1]]$result$span
  } else {
    info <- calls_named(run, "weasel_scope_info")
    assert(length(info) == 1L, recipe, "did not inspect the resolved Scope span")
    span <- info[[1]]$result$span
  }
  assert(identical(as.numeric(span), as.numeric(expected_span)),
         recipe, "resolved the wrong span for grid", grid)
  displayed <- which(vapply(run$calls, function(x) {
    identical(x$name, "print") && length(x$args) >= 1L &&
      is.numeric(x$args[[1]]) && is.null(dim(x$args[[1]])) &&
      identical(as.numeric(x$args[[1]]), as.numeric(span))
  }, logical(1)))
  selection <- which(vapply(run$calls, function(x) {
    x$name %in% c("weasel_reshape_to_wide", "weasel_summarize_subset",
                  "weasel_apply", "weasel_justify_subset")
  }, logical(1)))
  assert(length(displayed) == 1L && length(selection) > 0L &&
           displayed < min(selection),
         recipe, "did not print the actual span before selection")
}

scope_recipes <- c("result_scope_full", "result_scope_bounded")
real_recipes <- c(scope_recipes, plan_recipes, "result_plan_custom")
grid_panel <- expand.grid(id = 1:5, time = c(1L, 2L, 4L, 5L, 6L))
for (recipe in real_recipes) {
  default <- run_recipe(recipe, grid_panel)
  expected <- if (recipe == "result_scope_bounded") 1:10 else 1:6
  assert(identical(default$env$wave_grid, "consecutive"),
         recipe, "did not declare the illustrated consecutive grid")
  assert_grid_audit(default, recipe, "consecutive", expected)
  observed <- run_recipe(recipe, grid_panel, wave_grid = "observed")
  assert_grid_audit(observed, recipe, "observed", c(1L, 2L, 4L, 5L, 6L))
  weasel_clear_scope()
}

# biennial labels are not annual scheduling indices: two artificial
# missing years empty the strict/balanced presets, while the lenient
# preset can still retain people. observed resolves three actual waves
biennial <- expand.grid(id = 1:5, time = c(2008L, 2010L, 2012L))
presets <- c("anchored_strict", "anchored_balanced", "lenient_info_max")
for (recipe in plan_recipes) {
  annual <- run_recipe(recipe, biennial)
  assert_grid_audit(annual, recipe, "consecutive", 2008:2012)
  counts <- annual$env$p$plan$n_ids[match(presets, annual$env$p$plan$scenario)]
  assert(all(counts == c(0L, 0L, 5L)) &&
           identical(annual$env$chosen_scenario, "lenient_info_max") &&
           any(vapply(annual$warnings, inherits, logical(1), "weasel_empty_waves")),
         recipe, "did not expose the biennial consecutive-grid consequence")
  scheduled <- run_recipe(recipe, biennial, wave_grid = "observed")
  assert_grid_audit(scheduled, recipe, "observed", c(2008L, 2010L, 2012L))
  counts <- scheduled$env$p$plan$n_ids[
    match(presets, scheduled$env$p$plan$scenario)]
  assert(all(counts == 5L) &&
           setequal(unique(scheduled$env$analysis_data$id), 1:5),
         recipe, "did not retain the complete biennial panel on its observed grid")
}

# observed inference cannot recover a wave absent from everyone's data.
# a saved complete schedule lookup can instead supply deliberate indices
schedule <- data.frame(year = c(2008L, 2010L, 2012L, 2014L), time = 1:4)
absent_wave <- expand.grid(id = 1:5, year = c(2008L, 2012L, 2014L))
absent_wave$time <- schedule$time[match(absent_wave$year, schedule$year)]
assert(identical(sort(unique(absent_wave$time)), c(1L, 3L, 4L)) &&
         nrow(absent_wave) == 15L,
       "complete-schedule mapping added placeholder rows or compressed the missing wave")
inferred <- run_recipe("result_plan_full", absent_wave, wave_grid = "observed")
assert_grid_audit(inferred, "result_plan_full", "observed", c(1L, 3L, 4L))
strict_row <- match("anchored_strict", inferred$env$p$plan$scenario)
assert(inferred$env$p$plan$n_ids[strict_row] == 5L,
       "observed inference did not omit the globally absent scheduled wave")
scheduled <- run_recipe("result_plan_full", absent_wave)
assert_grid_audit(scheduled, "result_plan_full", "consecutive", 1:4)
strict_row <- match("anchored_strict", scheduled$env$p$plan$scenario)
assert(scheduled$env$p$plan$n_ids[strict_row] == 0L,
       "consecutive scheduling did not count the globally absent wave")
bounded_schedule <- suppressWarnings(weasel_plan(absent_wave, "id", "time",
  lower = 1L, upper = 4L, grid = "consecutive"))
assert(identical(bounded_schedule$span, 1:4) &&
         nrow(weasel_apply(bounded_schedule, "anchored_strict")) == 0L,
       "explicit bounds on complete-schedule indices did not preserve the absent wave")

# a row with an NA outcome is participation. no row is different, and
# neither grid selection nor custom criteria redefine item eligibility
custom_panel <- data.frame(
  id = c(rep("complete", 3), rep("one_missing", 2), rep("no_first", 2)),
  time = c(1:3, 1L, 3L, 2L, 3L),
  outcome = c(1, NA, 1, 1, 1, 1, 1)
)
custom <- run_recipe("result_plan_custom", custom_panel)
assert_grid_audit(custom, "result_plan_custom", "consecutive", 1:3)
criteria <- custom$env$criteria
assert(identical(names(criteria), c("scenario", "require_endpoints", "max_missing",
                                    "n_gap_max", "max_gap_len")) &&
         identical(criteria$scenario, c("custom_complete", "custom_one_missing")),
       "custom recipe does not pass the five supported fields and named alternatives")
custom_counts <- custom$env$p$plan$n_ids[
  match(criteria$scenario, custom$env$p$plan$scenario)]
assert(all(custom_counts == c(1L, 2L)) &&
         identical(custom$env$chosen_scenario, "custom_one_missing") &&
         setequal(unique(custom$env$analysis_data$id), c("complete", "one_missing")),
       "custom recipe did not extract its deliberately chosen named scenario")
assert(anyNA(custom$env$analysis_data$outcome) &&
         identical(unique(weasel_apply(custom$env$p, "custom_complete")$id),
                   "complete"),
       "custom recipe confused NA item values with absent participation rows")
assert(length(selection_calls(custom)) >= 3L &&
         all(vapply(selection_calls(custom), function(x) {
           identical(x$args[[2]], custom$env$chosen_scenario)
         }, logical(1))),
       "custom summary, extraction and methods text used different scenarios")

# the same structural rules also operate in one active Scope. compare
# respondent IDs rather than rendered counts or row order
chosen <- criteria[match(custom$env$chosen_scenario, criteria$scenario), ]
set_weasel_scope(custom_panel, "id", "time", grid = custom$env$wave_grid,
  lower = custom$env$p$lower, upper = custom$env$p$upper,
  require_endpoints = chosen$require_endpoints, max_missing = chosen$max_missing,
  n_gap_max = chosen$n_gap_max, max_gap_len = chosen$max_gap_len)
custom_scope <- weasel_reshape_to_wide()
assert(setequal(custom_scope$id, custom$env$analysis_data$id),
       "equivalent custom Scope and Plan constraints retained different respondents")
weasel_clear_scope()

for (choice in list(character(0), NA_character_, "not_a_scenario",
                     c("custom_one_missing", "custom_one_missing"))) {
  invalid <- run_recipe("result_plan_custom", custom_panel, chosen_scenario = choice)
  assert(inherits(invalid$error, "error") && length(selection_calls(invalid)) == 0L &&
           !exists("analysis_data", envir = invalid$env, inherits = FALSE),
         "custom recipe did not reject an invalid named choice before selection")
}
empty_custom <- run_recipe("result_plan_custom", sparse)
assert(inherits(empty_custom$error, "error") &&
         length(selection_calls(empty_custom)) == 0L &&
         !exists("analysis_data", envir = empty_custom$env, inherits = FALSE),
       "custom recipe did not stop an empty named scenario before selection")

# readiness is an API prerequisite, not an implicit coercion in a recipe
for (waves in list(as.character(1:3), factor(1:3), c(1, 1.5, 3), c(1, 2, Inf))) {
  invalid_panel <- data.frame(id = 1L, time = waves)
  for (recipe in c("result_scope_full", "result_plan_full")) {
    invalid <- run_recipe(recipe, invalid_panel)
    assert(inherits(invalid$error, "error") &&
             length(selection_calls(invalid)) == 0L &&
             length(calls_named(invalid, "weasel_reshape_to_wide")) == 0L,
           recipe, "did not reject unsupported wave coding before selection")
  }
}

unlink(snippet_dir, recursive = TRUE)
cat("guide semantics OK: scenario consistency, fixed-window completeness,",
    "empty/optional diagnostic guards, full sensitivity display, and",
    "Scope/Plan observation equivalence\n")
cat("guide semantics OK: explicit grid and span audits, biennial/absent-wave",
    "counterexamples, row-presence participation, and custom named criteria\n")
