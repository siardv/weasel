# regression tests for the 0.4.1 validation hardening: scenario-table
# normalization through labels, strict endpoint flags in both entry
# points, generator probability/shape/seed validation before any draws,
# and the tie_tolerance scalar contract

vh_panel <- function() {
  suppressMessages(
    generate_weasel_dummy_data(n_ids = 80, n_times = 8, seed = 5)
  )
}
vh_scen <- function(...) {
  base <- list(scenario = c("a", "b"), require_endpoints = FALSE,
               max_missing = c(0, 1), n_gap_max = c(0, 1),
               max_gap_len = c(0, 1))
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  do.call(data.frame, c(base, list(stringsAsFactors = FALSE)))
}

# ---- scenario tolerance normalization -------------------------------------

test_that("factor tolerances convert through labels, not level codes", {
  d <- vh_panel()
  fac <- vh_scen(max_missing = factor(c("0", "1")),
                 n_gap_max = factor(c("0", "1")),
                 max_gap_len = factor(c("0", "1")))
  num <- vh_scen()
  p_fac <- suppressMessages(
    weasel_plan(d, "id", "time", span = "full", scenarios = fac)
  )
  p_num <- suppressMessages(
    weasel_plan(d, "id", "time", span = "full", scenarios = num)
  )
  expect_identical(p_fac$plan$max_missing, c(0, 1))
  expect_identical(p_fac$plan$n_gap_max, c(0, 1))
  expect_identical(p_fac$plan$max_gap_len, c(0, 1))
  expect_identical(p_fac$plan$n_ids, p_num$plan$n_ids)
  expect_identical(p_fac$plan$ids, p_num$plan$ids)
})

test_that("numeric-looking character tolerances convert, Inf included", {
  s <- weasel:::.weasel_check_scenarios(
    vh_scen(scenario = c("a", "b"),
            max_missing = c("0", "1"),
            n_gap_max = c("1", "Inf"),
            max_gap_len = c("0", "Inf"))
  )
  expect_identical(s$max_missing, c(0, 1))
  expect_identical(s$n_gap_max, c(1, Inf))
  expect_identical(s$max_gap_len, c(0, Inf))
})

test_that("fractional factor tolerances are rejected, not truncated", {
  expect_error(
    weasel:::.weasel_check_scenarios(
      vh_scen(scenario = "a", require_endpoints = FALSE,
              max_missing = factor("1.5"), n_gap_max = 0, max_gap_len = 0)
    ),
    regexp = "integer-valued",
    class = "weasel_error"
  )
})

test_that("uninterpretable tolerance values are rejected naming the value", {
  expect_error(
    weasel:::.weasel_check_scenarios(
      vh_scen(scenario = "a", require_endpoints = FALSE,
              max_missing = factor("not-a-number"), n_gap_max = 0,
              max_gap_len = 0)
    ),
    regexp = "not interpretable.*not-a-number",
    class = "weasel_error"
  )
  expect_error(
    weasel:::.weasel_check_scenarios(
      vh_scen(scenario = "a", require_endpoints = FALSE,
              max_missing = "oops", n_gap_max = 0, max_gap_len = 0)
    ),
    regexp = "'max_missing'.*not interpretable",
    class = "weasel_error"
  )
})

test_that("missing tolerance values are still rejected", {
  expect_error(
    weasel:::.weasel_check_scenarios(
      vh_scen(scenario = c("a", "b"), max_missing = c(0, NA))
    ),
    class = "weasel_error"
  )
})

# ---- endpoint flags: scenario tables --------------------------------------

test_that("endpoint flags accept logicals and unambiguous imports only", {
  ok_logical <- weasel:::.weasel_check_scenarios(
    vh_scen(require_endpoints = c(TRUE, FALSE))
  )
  expect_identical(ok_logical$require_endpoints, c(TRUE, FALSE))

  ok_chr <- weasel:::.weasel_check_scenarios(
    vh_scen(require_endpoints = c("TRUE", "F"))
  )
  expect_identical(ok_chr$require_endpoints, c(TRUE, FALSE))

  ok_fac <- weasel:::.weasel_check_scenarios(
    vh_scen(require_endpoints = factor(c("TRUE", "FALSE")))
  )
  expect_identical(ok_fac$require_endpoints, c(TRUE, FALSE))

  ok_num <- weasel:::.weasel_check_scenarios(
    vh_scen(require_endpoints = c(1, 0))
  )
  expect_identical(ok_num$require_endpoints, c(TRUE, FALSE))
})

test_that("broad logical coercion of endpoint flags is rejected", {
  expect_error(
    weasel:::.weasel_check_scenarios(vh_scen(require_endpoints = c(2, 0))),
    regexp = "require_endpoints",
    class = "weasel_error"
  )
  expect_error(
    weasel:::.weasel_check_scenarios(
      vh_scen(require_endpoints = c("yes", "no"))
    ),
    regexp = "require_endpoints",
    class = "weasel_error"
  )
  expect_error(
    weasel:::.weasel_check_scenarios(vh_scen(require_endpoints = c("", ""))),
    class = "weasel_error"
  )
  expect_error(
    weasel:::.weasel_check_scenarios(
      vh_scen(require_endpoints = c(TRUE, NA))
    ),
    class = "weasel_error"
  )
})

# ---- endpoint flags: weasel_sensitivity -----------------------------------

test_that("weasel_sensitivity requires actual logical endpoint flags", {
  p <- suppressMessages(
    weasel_plan(vh_panel(), "id", "time", span = "full")
  )
  expect_error(
    weasel_sensitivity(p, require_endpoints = 2, max_missing = 0),
    regexp = "require_endpoints",
    class = "weasel_error"
  )
  expect_error(
    weasel_sensitivity(p, require_endpoints = "TRUE", max_missing = 0),
    regexp = "require_endpoints",
    class = "weasel_error"
  )
  expect_error(
    weasel_sensitivity(p, require_endpoints = c(TRUE, NA), max_missing = 0),
    class = "weasel_error"
  )
  sens <- weasel_sensitivity(p, require_endpoints = c(TRUE, FALSE),
                             max_missing = 0)
  expect_true(is.data.frame(sens) && nrow(sens) > 0)
})

# ---- generator validation --------------------------------------------------

test_that("generator probabilities are validated before any draws", {
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, prop_random = -1,
                               seed = 1),
    regexp = "prop_random.*\\[0, 1\\]",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5,
                               prop_item_missing = 2, seed = 1),
    regexp = "prop_item_missing",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, prop_attrition = 2,
                               seed = 1),
    regexp = "prop_attrition",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5,
                               prop_attention = 1.5, seed = 1),
    regexp = "prop_attention",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, prop_block = -0.1,
                               seed = 1),
    regexp = "prop_block",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5,
                               prop_random = c(0.1, 0.2), seed = 1),
    class = "weasel_error"
  )
})

test_that("attention shape parameters are validated", {
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5,
                               attention_scale = 0, seed = 1),
    regexp = "attention_scale",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5,
                               attention_scale = -2, seed = 1),
    regexp = "attention_scale",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5,
                               attention_center = Inf, seed = 1),
    regexp = "attention_center",
    class = "weasel_error"
  )
})

test_that("the seed is validated before set.seed is reached", {
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, seed = "abc"),
    regexp = "seed",
    class = "weasel_error"
  )
  expect_error(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, seed = 1.5),
    regexp = "seed",
    class = "weasel_error"
  )
  # NULL still draws and reports a seed; integer-valued doubles still work
  d <- suppressMessages(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, seed = 7)
  )
  d2 <- suppressMessages(
    generate_weasel_dummy_data(n_ids = 10, n_times = 5, seed = 7.0)
  )
  expect_identical(d, d2)
})

test_that("validation does not change what a valid seed generates", {
  # errors happen before any draw, so valid calls draw exactly as before
  d1 <- suppressMessages(
    generate_weasel_dummy_data(n_ids = 15, n_times = 6, seed = 99)
  )
  try(generate_weasel_dummy_data(n_ids = 15, n_times = 6, prop_random = -1,
                                 seed = 99), silent = TRUE)
  d2 <- suppressMessages(
    generate_weasel_dummy_data(n_ids = 15, n_times = 6, seed = 99)
  )
  expect_identical(d1, d2)
})

# ---- core_len clamp transparency ------------------------------------------

test_that("core_len clamping is reported, never silent", {
  d <- vh_panel()  # 8 observed waves, so the feasible range is 2:8
  old <- options(weasel.verbose = TRUE)
  on.exit(options(old), add = TRUE)
  expect_message(
    p_hi <- weasel_plan(d, "id", "time", span = "core", core_len = 99),
    regexp = "core_len"
  )
  expect_identical(length(p_hi$span), 8L)
  expect_message(
    p_lo <- weasel_plan(d, "id", "time", span = "core", core_len = 1),
    regexp = "core_len"
  )
  expect_identical(length(p_lo$span), 2L)
  # a feasible core_len stays free of clamp messages
  expect_no_message(
    weasel_plan(d, "id", "time", span = "core", core_len = 6),
    message = "core_len"
  )
})

test_that("the clamp message respects the verbose option", {
  d <- vh_panel()
  old <- options(weasel.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  expect_no_message(
    weasel_plan(d, "id", "time", span = "core", core_len = 99)
  )
})

# ---- tie_tolerance scalar contract ----------------------------------------

test_that("tie_tolerance must be a single non-negative number", {
  p <- suppressMessages(
    weasel_plan(vh_panel(), "id", "time", span = "full")
  )
  expect_error(
    weasel_compare_scenarios(p, tie_tolerance = c(0.01, 9)),
    regexp = "tie_tolerance",
    class = "weasel_error"
  )
  expect_error(
    weasel_compare_scenarios(p, tie_tolerance = "0.5"),
    regexp = "tie_tolerance",
    class = "weasel_error"
  )
  expect_error(
    weasel_compare_scenarios(p, tie_tolerance = NA_real_),
    class = "weasel_error"
  )
  expect_error(
    weasel_compare_scenarios(p, tie_tolerance = -1),
    class = "weasel_error"
  )
  cmp <- weasel_compare_scenarios(p, tie_tolerance = 0.01)
  expect_true(is.data.frame(cmp))
  # Inf remains the documented flag-everything extreme
  cmp_inf <- weasel_compare_scenarios(p, tie_tolerance = Inf)
  expect_true(all(cmp_inf$near_tie[!is.na(cmp_inf$score)]))
})
