test_that("scope pipeline recovers known patterns and counts", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  weasel_reshape_to_wide()
  v <- weasel_summarize_waves()

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

  # n_range filtering operates on observed-wave counts
  expect_true(all(weasel_filter_wave_summary(n_range = c(7, 8))$n >= 7))
  expect_lt(nrow(weasel_filter_wave_summary(n_range = c(7, 8))), nrow(v))
  expect_error(weasel_filter_wave_summary(n_range = 7), "length-2")
})

test_that("weasel_get_data_by_row returns matching respondents", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  weasel_reshape_to_wide()
  v <- weasel_summarize_waves()

  row_complete <- which(v$waves == paste(1:8, collapse = " "))
  sub <- weasel_get_data_by_row(row_complete)
  expect_setequal(unique(sub$id), c("a1", "a2"))
  expect_equal(nrow(sub), 16)
  expect_error(weasel_get_data_by_row(99), "out of range")

  # a vector of rows returns the union of matching respondents
  row_b1 <- which(v$waves == "1 2 . 4 5 6 7 8")
  both <- weasel_get_data_by_row(c(row_complete, row_b1))
  expect_setequal(unique(both$id), c("a1", "a2", "b1"))
})

test_that("interior gap constraints drop the right respondents", {
  d <- make_fixture()

  # gap = 1: c1 (gap of 2) must be dropped, b1 and e1 kept
  set_weasel_scope(d, "id", "time", size = 1, gap = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()
  pv <- weasel_reshape_to_wide()
  expect_false("c1" %in% pv$id)
  expect_true(all(c("a1", "b1", "e1") %in% pv$id))

  # n_gap = 1: e1 (three gaps) must also be dropped
  set_weasel_scope(d, "id", "time", size = 1, gap = 2, n_gap = 1)
  pv2 <- weasel_reshape_to_wide()
  expect_false("e1" %in% pv2$id)
  expect_true(all(c("b1", "c1", "d1") %in% pv2$id))
})

test_that("a single surviving respondent no longer breaks the summary", {
  one <- data.frame(id = "only", time = c(1, 2, 4), stringsAsFactors = FALSE)
  set_weasel_scope(one, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  # wave 3 has no observations at all, which now warns (once per scope)
  expect_warning(evaluate_weasel_scope(), class = "weasel_empty_waves")
  weasel_reshape_to_wide()
  v <- weasel_summarize_waves()
  expect_equal(nrow(v), 1)
  expect_equal(v$ids, 1L)
  expect_equal(v$n, 3L)
})

test_that("scope validates inputs eagerly and manages state cleanly", {
  d <- make_fixture()
  df <- d
  df$time <- factor(df$time)
  expect_error(set_weasel_scope(df, "id", "time"), "factor")
  expect_error(set_weasel_scope(d, "id", "nope"), "not found")

  # validation happens at set time, not several steps later
  expect_error(set_weasel_scope(d, "id", "time", gap = -1), "non-negative")
  expect_error(set_weasel_scope(d, "id", "time", n_gap = NA), "non-negative")
  expect_error(set_weasel_scope(d, "id", "time", lower = 5, upper = 2),
               ">= lower")
  expect_error(set_weasel_scope(d, "id", "time", size = 0), "positive")

  weasel_clear_scope()
  expect_error(evaluate_weasel_scope(), class = "weasel_error_no_scope")
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

test_that("scope pipeline supports observed grids", {
  d <- expand.grid(id = 1:10, time = seq(2, 14, by = 3))  # 2, 5, 8, 11, 14
  set_weasel_scope(d, "id", "time", grid = "observed", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_no_warning(evaluate_weasel_scope())
  pv <- weasel_reshape_to_wide()
  expect_equal(ncol(pv) - 1L, 5L)
  v <- weasel_summarize_waves()
  expect_equal(nrow(v), 1L)
  expect_equal(v$n, 5L)

  # the consecutive default warns about structurally empty waves,
  # once per scope
  set_weasel_scope(d, "id", "time", size = 1)
  expect_warning(evaluate_weasel_scope(), class = "weasel_empty_waves")
  expect_no_warning(weasel_reshape_to_wide())
})

test_that("weasel_scope_info reports settings and pipeline stage", {
  weasel_clear_scope()
  expect_output(weasel_scope_info(), "no active")

  d <- make_fixture()
  set_weasel_scope(d, "id", "time", gap = 1, size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  out <- capture.output(info <- weasel_scope_info())
  expect_true(any(grepl("stage:", out)))
  expect_true(any(grepl("not evaluated yet", out)))
  expect_equal(info$gap, 1L)
  expect_true(is.na(info$n_kept))

  weasel_reshape_to_wide()
  weasel_summarize_waves()
  out2 <- capture.output(info2 <- weasel_scope_info())
  expect_true(any(grepl("summarized", out2)))
  expect_false(is.na(info2$n_kept))
  expect_equal(length(info2$span), 8L)
})

test_that("duplicated rows warn in the scope pipeline too", {
  d <- make_fixture()
  dd <- rbind(d, d[1:3, ])
  set_weasel_scope(dd, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_warning(weasel_reshape_to_wide(), class = "weasel_duplicates")
})
