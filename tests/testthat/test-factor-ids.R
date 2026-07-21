# regression tests for factor-id handling in subset summaries (0.4.1):
# unused factor levels previously entered the grouping as phantom empty
# respondents, corrupting every headline statistic while n_ids stayed
# correct. summaries must be invariant to the id representation.

# the adjustment report's exact reproduction: levels A, B, C, D; a
# strict scenario retains only A and B, both complete over four waves
fid_data <- function(id_transform = identity) {
  d <- data.frame(
    id = rep(c("A", "B", "C", "D"), c(4, 4, 2, 2)),
    time = c(1:4, 1:4, 1:2, 3:4),
    var1 = c(1:4, 11:14, 21:22, 31:32),
    stringsAsFactors = FALSE
  )
  d$id <- id_transform(d$id)
  d
}
fid_strict <- function() {
  data.frame(scenario = "strict", require_endpoints = TRUE,
             max_missing = 0, n_gap_max = 0, max_gap_len = 0,
             stringsAsFactors = FALSE)
}
fid_summary <- function(d) {
  p <- suppressMessages(
    weasel_plan(d, "id", "time", span = "full", scenarios = fid_strict())
  )
  weasel_summarize_subset(p, "strict")
}

test_that("unused factor levels never corrupt the headline statistics", {
  s <- fid_summary(fid_data(function(x) factor(x, levels = c("A", "B", "C", "D"))))
  expect_identical(s$headline$n_ids, 2L)
  expect_identical(as.numeric(s$headline$mean_present), 4)
  expect_identical(as.numeric(s$headline$mean_missing), 0)
  expect_identical(as.numeric(s$headline$endpoint_rate), 1)
  expect_identical(as.integer(s$headline$min_present), 4L)
  expect_identical(as.integer(s$headline$max_present), 4L)
})

test_that("unused factor levels never appear in the missingness distribution", {
  s <- fid_summary(fid_data(function(x) factor(x, levels = c("A", "B", "C", "D"))))
  expect_identical(nrow(s$missing_distribution), 1L)
  expect_identical(s$missing_distribution$n_missing, 0L)
  expect_identical(s$missing_distribution$n_ids, 2L)
})

test_that("character, factor, and integer id representations agree", {
  reps <- list(
    character      = identity,
    factor_clean   = function(x) factor(x),
    factor_unused  = function(x) factor(x, levels = c("A", "B", "C", "D")),
    factor_reorder = function(x) factor(x, levels = c("D", "C", "B", "A"))
  )
  summaries <- lapply(reps, function(tr) fid_summary(fid_data(tr)))
  ref <- summaries$character
  for (nm in names(summaries)[-1]) {
    s <- summaries[[nm]]
    expect_identical(s$headline[-1][sapply(s$headline[-1], is.numeric)],
                     ref$headline[-1][sapply(ref$headline[-1], is.numeric)],
                     info = nm)
    expect_identical(s$per_wave_coverage, ref$per_wave_coverage, info = nm)
    expect_identical(s$missing_distribution, ref$missing_distribution,
                     info = nm)
    expect_identical(weasel_subset_to_sentence(s),
                     weasel_subset_to_sentence(ref), info = nm)
  }
})

test_that("integer ids summarize like their character equivalents", {
  d_int <- fid_data()
  d_int$id <- match(d_int$id, c("A", "B", "C", "D"))
  s_int <- fid_summary(d_int)
  s_chr <- fid_summary(fid_data())
  num <- function(h) h[sapply(h, is.numeric)]
  expect_identical(num(s_int$headline[-1]), num(s_chr$headline[-1]))
  expect_identical(s_int$missing_distribution, s_chr$missing_distribution)
})

test_that("a retained-but-incomplete respondent keeps correct statistics", {
  # balanced scenario on a factor with unused levels: retained ids have
  # differing presence, so means and the distribution are non-trivial
  d <- data.frame(
    id = factor(rep(c("A", "B", "C"), c(4, 3, 1)),
                levels = c("A", "B", "C", "ghost1", "ghost2")),
    time = c(1:4, c(1, 2, 4), 2),
    stringsAsFactors = FALSE
  )
  balanced <- data.frame(scenario = "balanced", require_endpoints = TRUE,
                         max_missing = 1, n_gap_max = 1, max_gap_len = 1,
                         stringsAsFactors = FALSE)
  p <- suppressMessages(
    weasel_plan(d, "id", "time", span = "full", scenarios = balanced)
  )
  s <- weasel_summarize_subset(p, "balanced")
  expect_identical(s$headline$n_ids, 2L)
  expect_identical(as.numeric(s$headline$mean_present), 3.5)
  expect_identical(as.numeric(s$headline$endpoint_rate), 1)
  expect_identical(as.integer(s$headline$min_present), 3L)
  expect_identical(s$missing_distribution$n_ids, c(1L, 1L))
  expect_identical(s$missing_distribution$n_missing, c(0L, 1L))
})

test_that("the sentence reports the corrected values", {
  s <- fid_summary(fid_data(function(x) factor(x, levels = c("A", "B", "C", "D"))))
  txt <- weasel_subset_to_sentence(s)
  expect_match(txt, "selects 2 respondent(s)", fixed = TRUE)
  expect_match(txt, "Mean observed waves: 4.000 (missing: 0.000)",
               fixed = TRUE)
  expect_match(txt, "Endpoint rate: 1.000", fixed = TRUE)
  expect_match(txt, "2 respondent(s) have 0 missing wave(s)", fixed = TRUE)
})

test_that("plan-side metrics and summaries agree for factor ids", {
  # cross-path consistency: the recomputed summary must match the plan's
  # stored metrics whatever the id representation
  d <- fid_data(function(x) factor(x, levels = c("A", "B", "C", "D")))
  p <- suppressMessages(
    weasel_plan(d, "id", "time", span = "full", scenarios = fid_strict())
  )
  s <- weasel_summarize_subset(p, "strict")
  row <- p$plan[p$plan$scenario == "strict", ]
  expect_identical(as.numeric(s$headline$mean_present),
                   as.numeric(row$mean_present))
  expect_identical(as.numeric(s$headline$endpoint_rate),
                   as.numeric(row$endpoint_rate))
  expect_identical(s$headline$n_ids, as.integer(row$n_ids))
})

# phase 5 institutionalization: representation invariance must hold in
# BOTH pipelines, not only in plan-side summaries where the 0.4.1
# incident surfaced; the same panel goes through the scope pipeline
# under every id representation, and the cross-pipeline equivalence
# property is asserted under factor ids as well

test_that("scope-pipeline retention and summaries are invariant to the id representation", {
  scope_run <- function(d) {
    suppressMessages(
      set_weasel_scope(d, "id", "time", max_missing = 0, max_gap_len = 0,
                       n_gap_max = 0, require_endpoints = TRUE)
    )
    on.exit(weasel_clear_scope(), add = TRUE)
    pv <- suppressMessages(suppressWarnings(weasel_reshape_to_wide()))
    sw <- suppressMessages(weasel_summarize_waves())
    list(ids = sort(as.character(pv$id)), sw = sw)
  }
  reps <- list(
    character      = identity,
    factor_clean   = function(x) factor(x),
    factor_unused  = function(x) factor(x, levels = c("A", "B", "C", "D", "ghost")),
    factor_reorder = function(x) factor(x, levels = c("D", "C", "B", "A"))
  )
  runs <- lapply(reps, function(tr) scope_run(fid_data(tr)))
  for (nm in names(runs)) {
    expect_identical(runs[[nm]]$ids, c("A", "B"), info = nm)
  }
  for (nm in names(runs)[-1]) {
    expect_identical(runs[[nm]]$sw, runs$character$sw, info = nm)
  }

  d_int <- fid_data()
  d_int$id <- match(d_int$id, c("A", "B", "C", "D"))
  r_int <- scope_run(d_int)
  expect_identical(r_int$ids, c("1", "2"))
  expect_identical(r_int$sw, runs$character$sw)
})

test_that("scope and plan pipelines agree for factor ids with unused levels", {
  for (tr in list(function(x) factor(x, levels = c("A", "B", "C", "D")),
                  function(x) factor(x, levels = c("D", "C", "B", "A")))) {
    d <- fid_data(tr)
    p <- suppressMessages(
      weasel_plan(d, "id", "time", span = "full", scenarios = fid_strict())
    )
    suppressMessages(
      set_weasel_scope(d, "id", "time", max_missing = 0, max_gap_len = 0,
                       n_gap_max = 0, require_endpoints = TRUE)
    )
    pv <- suppressMessages(suppressWarnings(weasel_reshape_to_wide()))
    weasel_clear_scope()
    expect_setequal(as.character(pv$id), as.character(p$plan$ids[[1]]))
  }
})
