test_that("scope pipeline recovers known patterns and counts", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  reshape_to_wide()
  v <- summarize_waves()

  expect_s3_class(v, "data.frame")
  # 7 respondents, 6 distinct patterns (a1 and a2 share one)
  expect_equal(sum(v$ids), 7)
  expect_equal(nrow(v), 6)

  # n is the number of observed waves per pattern, not a constant
  expect_gt(length(unique(v$n)), 1)
  complete <- v[v$waves == paste(1:8, collapse = " "), ]
  expect_equal(complete$ids, 2)
  expect_equal(complete$n, 8)

  # the most common pattern is first
  expect_equal(v$ids[1], max(v$ids))

  # n_range filtering now operates on observed-wave counts
  expect_true(all(filter_wave_summary(n_range = c(7, 8))$n >= 7))
  expect_lt(nrow(filter_wave_summary(n_range = c(7, 8))), nrow(v))
})

test_that("get_data_by_row returns matching respondents", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  reshape_to_wide()
  v <- summarize_waves()

  row_complete <- which(v$waves == paste(1:8, collapse = " "))
  sub <- get_data_by_row(row_complete)
  expect_setequal(unique(sub$id), c("a1", "a2"))
  expect_equal(nrow(sub), 16)
  expect_error(get_data_by_row(99), "out of range")
})

test_that("interior gap constraints drop the right respondents", {
  d <- make_fixture()

  # gap = 1: c1 (gap of 2) must be dropped, b1 and e1 kept
  set_weasel_scope(d, "id", "time", size = 1, gap = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  pv <- reshape_to_wide()
  expect_false("c1" %in% pv$id)
  expect_true(all(c("a1", "b1", "e1") %in% pv$id))

  # n_gap = 1: e1 (three gaps) must also be dropped
  set_weasel_scope(d, "id", "time", size = 1, gap = 2, n_gap = 1)
  pv2 <- reshape_to_wide()
  expect_false("e1" %in% pv2$id)
  expect_true(all(c("b1", "c1", "d1") %in% pv2$id))
})

test_that("a single surviving respondent no longer breaks the summary", {
  one <- data.frame(id = "only", time = c(1, 2, 4), stringsAsFactors = FALSE)
  set_weasel_scope(one, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  reshape_to_wide()
  v <- summarize_waves()
  expect_equal(nrow(v), 1)
  expect_equal(v$ids, 1L)
  expect_equal(v$n, 3L)
})

test_that("scope validates inputs and state cleanly", {
  d <- make_fixture()
  df <- d
  df$time <- factor(df$time)
  expect_error(set_weasel_scope(df, "id", "time"), "factor")
  expect_error(set_weasel_scope(d, "id", "nope"), "not found")

  weasel_clear_scope()
  expect_error(evaluate_weasel_scope(), "no scope set")

  set_weasel_scope(d, "id", "time")
  expect_error(set_weasel_scope(d, "id", "time", override = FALSE),
               "already exists")
  expect_true(weasel_clear_scope())
  expect_false(weasel_clear_scope())
})

test_that("scope state never touches the global environment", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time")
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_false(exists("weasel_env", envir = globalenv(), inherits = FALSE))
})

test_that("a too-short span fails with a clear message", {
  tiny <- data.frame(id = rep(1:5, each = 2), time = rep(1:2, 5))
  set_weasel_scope(tiny, "id", "time", size = 3)
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_error(evaluate_weasel_scope(), "no valid window size")
})

test_that("legacy stubs still run as no-ops", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time")
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  expect_silent(invisible(generate_sets()))
  expect_silent(invisible(filter_sets()))
})
