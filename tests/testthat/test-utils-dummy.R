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

test_that("wave validation rejects infinities and integer overflow cleanly", {
  for (value in c(Inf, -Inf)) {
    expect_no_warning(expect_error(
      weasel:::.weasel_check_wave(c(1, value, NA, NaN), "time"),
      "column 'time'.*finite", class = "weasel_error"
    ))
  }
  limit <- .Machine$integer.max
  for (value in c(limit + 1, -limit - 1, 1e20, -1e20)) {
    expect_no_warning(expect_error(
      weasel:::.weasel_check_wave(c(1, value, NA, NaN), "time"),
      "column 'time'.*between", class = "weasel_error"
    ))
  }
})

test_that("wave validation preserves integer boundaries and rounding tolerance", {
  limit <- .Machine$integer.max
  waves <- c(limit, -limit, limit, 2 + 5e-9, -2 - 5e-9, 1e-8, NA, NaN)
  expect_no_warning(expect_identical(
    weasel:::.weasel_check_wave(waves, "time"),
    c(-limit, -2L, 0L, 2L, limit)
  ))
  expect_no_warning(expect_error(
    weasel:::.weasel_check_wave(c(1, 1.1e-8), "time"),
    "column 'time'.*integer-valued", class = "weasel_error"
  ))
  expect_no_warning(expect_error(
    weasel:::.weasel_check_wave(c(NA_real_, NaN), "time"),
    "column 'time'.*no non-missing values", class = "weasel_error"
  ))
})

test_that("scalar integer validators reject overflow before conversion", {
  limit <- .Machine$integer.max
  for (value in c(limit + 1, 1e100)) {
    expect_no_warning(expect_error(
      weasel:::.weasel_check_count(value, "count"),
      "count.*between 0 and 2147483647", class = "weasel_error"
    ))
  }
  for (value in c(limit + 1, -limit - 1, 1e100, -1e100)) {
    expect_no_warning(expect_error(
      weasel:::.weasel_check_bound(value, "bound"),
      "bound.*between -2147483647 and 2147483647", class = "weasel_error"
    ))
  }
})

test_that("scalar integer validators preserve their existing input rules", {
  limit <- .Machine$integer.max
  values <- list(0, 2L, as.double(limit), 2 + 5e-9, 1e-8)
  expected <- c(0L, 2L, limit, 2L, 0L)
  for (check in list(weasel:::.weasel_check_count, weasel:::.weasel_check_bound)) {
    expect_null(check(NULL, "arg"))
    for (i in seq_along(values)) {
      expect_no_warning(expect_identical(check(values[[i]], "arg"), expected[[i]]))
    }
    for (value in list(numeric(), c(1, 2), "1", factor("1"), TRUE,
                       NA_real_, NaN, Inf, -Inf, 1.5, 1.1e-8)) {
      expect_no_warning(expect_error(check(value, "arg"), "arg.*single",
                                     class = "weasel_error"))
    }
  }
  values <- c(-limit, -2 - 5e-9, -1e-8)
  expected <- c(-limit, -2L, 0L)
  for (i in seq_along(values)) {
    expect_no_warning(expect_identical(
      weasel:::.weasel_check_bound(values[[i]], "bound"), expected[[i]]
    ))
    expect_no_warning(expect_error(
      weasel:::.weasel_check_count(values[[i]], "count"), "count.*non-negative",
      class = "weasel_error"
    ))
  }
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

test_that("dummy data preserves tiny ID intervals at integer boundaries", {
  limit <- .Machine$integer.max
  intervals <- list(c(-limit, -limit + 1L), c(-1L, 0L, 1L), 0L,
                    limit, c(limit - 1L, limit))
  for (ids in intervals) {
    reference <- generate_weasel_dummy_data(
      n_ids = length(ids), n_times = 4, n_vars = 2, id_start = 1, seed = 7
    )
    expect_no_warning(generated <- generate_weasel_dummy_data(
      n_ids = length(ids), n_times = 4, n_vars = 2,
      id_start = ids[1L], seed = 7
    ))
    expect_identical(generated$id, ids[reference$id])
    expect_identical(unique(generated$id), ids)
    expect_identical(generated[-1L], reference[-1L])
  }
})

test_that("invalid ID intervals fail before seed messages and preserve RNG state", {
  old <- options(weasel.verbose = TRUE)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit({
    options(old)
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  RNGkind("Wichmann-Hill", "Box-Muller", sample.kind = "Rejection")
  set.seed(61)
  before_seed <- .Random.seed
  before_kind <- RNGkind()
  limit <- .Machine$integer.max
  for (has_seed in c(TRUE, FALSE)) {
    if (!has_seed) rm(".Random.seed", envir = globalenv())
    for (n_ids in 2:3) {
      expect_no_message(expect_no_warning(expect_error(
        generate_weasel_dummy_data(n_ids = n_ids, n_times = 3, n_vars = 1,
                                   id_start = limit - n_ids + 2L),
        "id_start.*n_ids.*2147483647", class = "weasel_error"
      )))
      expect_identical(
        exists(".Random.seed", envir = globalenv(), inherits = FALSE), has_seed
      )
      if (has_seed) expect_identical(.Random.seed, before_seed)
      expect_identical(RNGkind(), before_kind)
    }
  }
})

test_that("dummy data is invariant to the caller's RNG kind", {
  d_default <- generate_weasel_dummy_data(n_ids = 20, n_times = 6, seed = 42)

  old <- RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(old))), add = TRUE)
  suppressWarnings(
    RNGkind("Wichmann-Hill", "Box-Muller", sample.kind = "Rounding")
  )
  legacy_kind <- RNGkind()
  d_legacy <- generate_weasel_dummy_data(n_ids = 20, n_times = 6, seed = 42)

  # same seed, same panel, regardless of the caller's sampler
  expect_identical(d_default, d_legacy)
  # and the caller's non-default kind survives the call untouched
  expect_identical(RNGkind(), legacy_kind)
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
