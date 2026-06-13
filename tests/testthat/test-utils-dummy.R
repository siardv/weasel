test_that(".weasel_seq_int never counts downwards", {
  expect_identical(weasel:::.weasel_seq_int(2, 5), 2:5)
  expect_identical(weasel:::.weasel_seq_int(5, 2), integer(0))
  expect_identical(weasel:::.weasel_seq_int(NA, 2), integer(0))
})

test_that("interior gap metrics ignore leading/trailing absence", {
  g <- weasel:::.weasel_interior_gaps(c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(g$n_gap, 1L)
  expect_equal(g$max_gap, 2L)
  expect_equal(g$n_present, 2L)

  # full-span variant counts edge runs as gaps
  g2 <- weasel:::.weasel_rle_gaps(c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(g2$n_gap, 3L)
})

test_that("wave column validation rejects factors and non-integers", {
  expect_error(weasel:::.weasel_check_wave(factor(1:3)), "factor")
  expect_error(weasel:::.weasel_check_wave(c(1, 2.5)), "integer-valued")
  expect_error(weasel:::.weasel_check_wave(letters[1:3]), "numeric")
  expect_identical(weasel:::.weasel_check_wave(c(3, 1, 2, NA)), 1:3)
})

test_that("dummy data has genuine wave-level (row) missingness", {
  d <- generate_weasel_dummy_data(n_ids = 200, n_times = 10, seed = 42)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("id", "time", "var1") %in% names(d)))
  # the grid must be incomplete: missing waves are absent rows
  expect_lt(nrow(d), 200 * 10)
  # but every respondent keeps at least one observed wave
  expect_equal(length(unique(d$id)), 200)
  expect_false(any(duplicated(d[c("id", "time")])))
})

test_that("dummy data is reproducible and RNG-neutral", {
  d1 <- generate_weasel_dummy_data(n_ids = 30, n_times = 6, seed = 7)
  d2 <- generate_weasel_dummy_data(n_ids = 30, n_times = 6, seed = 7)
  expect_identical(d1, d2)

  # the caller's RNG stream is unaffected by the call
  set.seed(123)
  x1 <- rnorm(3)
  set.seed(123)
  invisible(generate_weasel_dummy_data(n_ids = 10, n_times = 5, seed = 1))
  x2 <- rnorm(3)
  expect_identical(x1, x2)
})

test_that("dummy data validates its arguments", {
  expect_error(generate_weasel_dummy_data(n_ids = 0), "n_ids")
  expect_error(generate_weasel_dummy_data(n_times = 2), "n_times")
})
