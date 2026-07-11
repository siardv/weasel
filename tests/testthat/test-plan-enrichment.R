# stage 4 contracts: explicit bounds, inspectable span selection,
# qualified recommendations, population counts, data fingerprints, and
# stable pattern ids

test_that("explicit lower/upper bounds fix the window a priori", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", lower = 3, upper = 6)
  expect_equal(p$span, 3:6)
  expect_identical(p$span_reason, "explicit")
  expect_null(p$span_candidates)

  # justification reports a design decision, not a span rule
  txt <- weasel_justify_subset(p, "lenient")
  expect_match(txt, "fixed a priori", fixed = TRUE)
  expect_match(txt, "waves 3 to 6", fixed = TRUE)

  # supplying both span and bounds is an error
  expect_error(weasel_plan(d, "id", "time", span = "core", lower = 3),
               "not both")
  # bounds are strict integers
  expect_error(weasel_plan(d, "id", "time", lower = 2.5), "fractional")

  # observed grids: effective bounds are the grid waves in range
  b <- expand.grid(id = 1:10, time = seq(2008, 2020, 2))
  pb <- weasel_plan(b, "id", "time", grid = "observed",
                    lower = 2009, upper = 2017)
  expect_equal(pb$span, c(2010L, 2012L, 2014L, 2016L))
  expect_equal(pb$lower, 2010L)
  expect_equal(pb$upper, 2016L)
})

test_that("core-window candidates are stored and ties warn", {
  # two identical 4-wave coverage blocks: 1:4 and 7:10
  d <- expand.grid(id = 1:10, time = c(1:4, 7:10))
  expect_warning(
    p <- weasel_plan(d, "id", "time", span = "core", core_len = 4),
    class = "weasel_tied_windows"
  )
  expect_equal(p$lower, 1L)  # earliest tied window wins, loudly
  cand <- p$span_candidates
  expect_s3_class(cand, "data.frame")
  expect_named(cand, c("lower", "upper", "coverage", "chosen"))
  expect_equal(sum(cand$chosen), 1L)
  expect_true(cand$chosen[cand$lower == 1])
  expect_equal(max(cand$coverage), 40L)
  expect_gt(sum(cand$coverage == max(cand$coverage)), 1L)

  # a unique best window stays silent and is marked chosen
  d2 <- generate_weasel_dummy_data(n_ids = 60, n_times = 9, seed = 91)
  p2 <- weasel_plan(d2, "id", "time", span = "core")
  expect_equal(sum(p2$span_candidates$chosen), 1L)
  chosen <- p2$span_candidates[p2$span_candidates$chosen, ]
  expect_equal(chosen$lower, p2$lower)
  expect_equal(chosen$upper, p2$upper)
})

test_that("scores expose weights and components; near ties are flagged", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  cmp <- weasel_compare_scenarios(p)

  w <- attr(cmp, "weights")
  comp <- attr(cmp, "score_components")
  expect_named(w, c("coverage", "endpoints", "size", "missing", "gaps"))
  expect_s3_class(comp, "data.frame")
  # the decomposition reconstructs the score exactly
  recomposed <- comp$coverage + comp$endpoints + comp$size +
    comp$missing + comp$gaps
  expect_equal(recomposed, cmp$score)

  # two structurally identical scenarios are an exact near-tie
  twins <- data.frame(
    scenario = c("twin_a", "twin_b"),
    require_endpoints = FALSE,
    max_missing = 2, n_gap_max = 2, max_gap_len = 2
  )
  pt <- weasel_plan(d, "id", "time", span = "full", scenarios = twins)
  ct <- weasel_compare_scenarios(pt)
  expect_equal(sum(ct$recommended), 1L)
  expect_true(all(ct$near_tie))
  s <- weasel_compare_to_sentence(ct)
  expect_match(s, "declared weights")
  expect_match(s, "not unique")

  expect_error(weasel_compare_scenarios(p, tie_tolerance = -1),
               "non-negative")
})

test_that("plans record their population and print the denominator", {
  core_ids <- do.call(rbind, lapply(1:20, function(i) {
    data.frame(id = i, time = 5:8)
  }))
  early <- do.call(rbind, lapply(21:22, function(i) {
    data.frame(id = i, time = 1:2)
  }))
  d <- rbind(core_ids, early)
  p <- weasel_plan(d, "id", "time", span = "core", core_len = 4)

  expect_equal(p$population$n_ids_data, 22L)
  expect_equal(p$population$n_ids_in_span, 20L)
  expect_identical(p$population$denominator, "observed_in_span")
  out <- capture.output(print(p))
  expect_true(any(grepl("population: 20 of 22", out)))

  txt <- weasel_justify_subset(p, "lenient")
  expect_match(txt, "planning population comprised 20", fixed = TRUE)
})

test_that("fingerprints catch reunions with mismatched data", {
  d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 92)
  p <- weasel_plan(d, "id", "time", span = "full", keep_data = FALSE)

  # the original data passes silently
  expect_no_warning(sub <- weasel_apply(p, "lenient", data = d))
  expect_gt(nrow(sub), 0)

  # structurally different data warns, in every reunion path
  d_bad <- d[-(1:5), ]
  expect_warning(weasel_apply(p, "lenient", data = d_bad),
                 class = "weasel_data_mismatch")
  expect_warning(weasel_summarize_subset(p, "lenient", data = d_bad),
                 class = "weasel_data_mismatch")
  expect_warning(weasel_selectivity(p, "anchored_strict", data = d_bad),
                 class = "weasel_data_mismatch")

  # plans without a fingerprint (older versions) stay silent
  legacy <- p
  legacy$fingerprint <- NULL
  expect_no_warning(weasel_apply(legacy, "lenient", data = d_bad))
})

test_that("pattern ids survive filtering and drive extraction", {
  d <- make_fixture()
  suppressMessages(set_weasel_scope(d, "id", "time", min_present = 1))
  on.exit(weasel_clear_scope(), add = TRUE)
  suppressMessages(weasel_reshape_to_wide())
  v <- weasel_summarize_waves()

  expect_equal(v$pattern, seq_len(nrow(v)))

  f <- weasel_filter_wave_summary(n_range = c(4, 6))
  expect_true(all(f$pattern %in% v$pattern))
  expect_lt(nrow(f), nrow(v))

  # a pattern id taken from the FILTERED table selects exactly the
  # respondents carrying that pattern (this was the pre-0.4 trap:
  # visible row 1 of a filtered table was not view row 1)
  target <- f$pattern[1]
  got <- weasel_get_data_by_row(target)
  scope_env <- weasel:::the$scope
  expected_ids <- scope_env$pivot$id[scope_env$waves_by_id == v$waves[target]]
  expect_setequal(unique(got$id), expected_ids)

  # pattern strings work as selectors too
  got2 <- weasel_get_data_by_row(v$waves[target])
  expect_equal(got, got2)

  # fractional ids are rejected
  expect_error(weasel_get_data_by_row(1.5), "pattern ids")
})
