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

test_that("vectorized gap metrics match the rle reference implementation", {
  set.seed(99)
  for (rep in 1:5) {
    n <- 40L
    L <- 12L
    m <- matrix(runif(n * L) < 0.6, n, L)
    for (r in which(rowSums(m) == 0L)) m[r, sample.int(L, 1)] <- TRUE

    ref <- t(vapply(seq_len(n), function(r) {
      g <- weasel:::.weasel_interior_gaps(m[r, ])
      c(g$n_present, g$n_gap, g$max_gap)
    }, numeric(3)))

    idx <- which(m, arr.ind = TRUE)
    got <- weasel:::.weasel_gap_metrics(idx[, 1], idx[, 2], L)
    got <- got[order(got$id), ]

    expect_equal(got$id, seq_len(n))
    expect_equal(got$n_present, as.integer(ref[, 1]))
    expect_equal(got$n_gap, as.integer(ref[, 2]))
    expect_equal(got$max_gap, as.integer(ref[, 3]))
    expect_equal(got$has_lower, unname(m[, 1]))
    expect_equal(got$has_upper, unname(m[, L]))
  }

  # empty input yields an empty, well-formed frame
  e <- weasel:::.weasel_gap_metrics(integer(0), integer(0), 5L)
  expect_equal(nrow(e), 0L)
  expect_true(all(c("n_present", "n_gap", "max_gap") %in% names(e)))
})

test_that("wave column validation rejects factors and non-integers", {
  expect_error(weasel:::.weasel_check_wave(factor(1:3)), "factor")
  expect_error(weasel:::.weasel_check_wave(c(1, 2.5)), "integer-valued")
  expect_error(weasel:::.weasel_check_wave(letters[1:3]), "numeric")
  expect_identical(weasel:::.weasel_check_wave(c(3, 1, 2, NA)), 1:3)
})

test_that("weasel errors and warnings are classed conditions", {
  err <- tryCatch(weasel:::.weasel_check_wave(letters[1:3]),
                  error = function(e) e)
  expect_s3_class(err, "weasel_error")

  wrn <- tryCatch(weasel:::.weasel_warn("hello", class = "weasel_test"),
                  warning = function(w) w)
  expect_s3_class(wrn, "weasel_warning")
  expect_s3_class(wrn, "weasel_test")
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

test_that("dummy data is invariant to the caller's RNG kind", {
  d_default <- generate_weasel_dummy_data(n_ids = 20, n_times = 6, seed = 42)

  old <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old[1], old[2], old[3])), add = TRUE)
  suppressWarnings(
    RNGkind("Wichmann-Hill", "Box-Muller", sample.kind = "Rounding")
  )
  d_legacy <- generate_weasel_dummy_data(n_ids = 20, n_times = 6, seed = 42)

  # same seed, same panel, regardless of the caller's sampler
  expect_identical(d_default, d_legacy)
  # and the caller's non-default kind survives the call untouched
  expect_identical(RNGkind(), c("Wichmann-Hill", "Box-Muller", "Rounding"))
})

test_that("dummy data supports explicit wave schedules", {
  sched <- seq(2008L, 2020L, by = 2L)
  b <- generate_weasel_dummy_data(n_ids = 25, waves = sched, seed = 3)
  expect_true(all(unique(b$time) %in% sched))
  expect_equal(length(unique(b$id)), 25)

  # positions map to labels: same seed, same participation pattern
  ref <- generate_weasel_dummy_data(n_ids = 25, n_times = length(sched),
                                    seed = 3)
  expect_identical(sched[match(ref$time, seq_along(sched))], b$time)

  p <- weasel_plan(b, "id", "time", span = "full", grid = "observed")
  expect_equal(p$span, sched)

  expect_error(generate_weasel_dummy_data(waves = c(1, 2)), "more than 2")
})

test_that("dummy data validates its arguments", {
  expect_error(generate_weasel_dummy_data(n_ids = 0), "n_ids")
  expect_error(generate_weasel_dummy_data(n_times = 2), "n_times")
})
