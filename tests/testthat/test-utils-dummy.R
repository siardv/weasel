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

test_that("dummy schedules preserve signed boundaries and normalized labels", {
  limit <- .Machine$integer.max
  schedules <- list(c(-limit, 0L, limit), c(limit - 2L, limit - 1L, limit),
                    c(-limit, -limit + 1L, -limit + 2L),
                    c(2 + 5e-9, -2 - 5e-9, 1e-8, 2L, -1e-8))
  labels <- list(c(-limit, 0L, limit), c(limit - 2L, limit - 1L, limit),
                c(-limit, -limit + 1L, -limit + 2L), c(-2L, 0L, 2L))
  for (i in seq_along(schedules)) {
    reference <- generate_weasel_dummy_data(
      n_ids = 3, n_times = 3, n_vars = 2, seed = 7
    )
    reference$time <- labels[[i]][reference$time]
    before <- serialize(schedules[[i]], NULL)
    expect_no_warning(actual <- generate_weasel_dummy_data(
      n_ids = 3, waves = schedules[[i]], n_vars = 2, seed = 7
    ))
    expect_identical(actual, reference)
    expect_type(actual$time, "integer")
    expect_identical(serialize(schedules[[i]], NULL), before)
  }
})

test_that("invalid schedule ranges fail before RNG entry without dropping labels", {
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
    for (value in c(limit + 1, -limit - 1,
                    .Machine$double.xmax, -.Machine$double.xmax)) {
      for (waves in list(value, c(1:3, value), c(value, 1, 2))) {
        expect_no_message(expect_no_warning(expect_error(
          generate_weasel_dummy_data(n_ids = 2, n_vars = 1, waves = waves),
          "waves.*between -2147483647 and 2147483647", class = "weasel_error"
        )))
        expect_identical(
          exists(".Random.seed", envir = globalenv(), inherits = FALSE), has_seed
        )
        if (has_seed) expect_identical(.Random.seed, before_seed)
        expect_identical(RNGkind(), before_kind)
      }
    }
  }
})

test_that("dummy schedule validation keeps existing rules and argument order", {
  overflow <- .Machine$integer.max + 1
  for (waves in list(numeric(), "1", factor(1:3), TRUE, list(1:3), 1 + 0i,
                     c(1:3, NA), c(1:3, NaN), c(1:3, Inf), c(1:3, -Inf),
                     c(1:3, 1.1e-8), c(overflow, NA), c(overflow, Inf),
                     c(overflow, 1.5))) {
    expect_no_warning(expect_error(
      generate_weasel_dummy_data(n_ids = 2, n_vars = 1, waves = waves),
      "waves must be integer-valued wave labels", class = "weasel_error"
    ))
  }
  for (waves in list(1:2, c(1, 1 + 5e-9, 2))) {
    expect_no_warning(expect_error(
      generate_weasel_dummy_data(n_ids = 2, waves = waves),
      "waves must contain more than 2 distinct", class = "weasel_error"
    ))
  }
  for (arg in c("n_times", "prop_random", "attention_scale", "seed")) {
    args <- list(n_ids = 2, n_vars = 1, waves = c(1:3, overflow))
    args[[arg]] <- if (arg %in% c("n_times", "seed")) overflow else -1
    expect_no_warning(expect_error(
      do.call(generate_weasel_dummy_data, args), arg, class = "weasel_error"
    ))
  }
  expect_no_warning(expect_error(
    generate_weasel_dummy_data(n_ids = NULL, waves = c(1:3, overflow)),
    "must not be NULL", class = "weasel_error"
  ))
  expect_no_warning(expect_error(
    generate_weasel_dummy_data(n_ids = 0, waves = overflow,
                               block_duration_range = c(0, 0)),
    "waves.*between", class = "weasel_error"
  ))
  reference <- generate_weasel_dummy_data(n_ids = 2, n_times = 3, seed = 7)
  for (n_times in 0:2) {
    expect_no_warning(actual <- generate_weasel_dummy_data(
      n_ids = 2, n_times = n_times, waves = 1:3, seed = 7
    ))
    expect_identical(actual, reference)
  }
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

test_that("fixed block durations create the requested contiguous missingness", {
  for (duration in 1:3) {
    expect_no_warning(d <- generate_weasel_dummy_data(
      n_ids = 6, n_times = 8, n_vars = 1, seed = 7,
      prop_random = 0, prop_attention = 0, prop_attrition = 0,
      prop_block = 1, block_duration_range = rep(duration, 2),
      prop_item_missing = 0
    ))
    expect_identical(length(unique(d$id)), 6L)
    for (times in split(d$time, d$id)) {
      missing <- setdiff(1:8, times)
      expect_gte(min(missing), 2L)
      expect_identical(missing, seq.int(min(missing), max(missing)))
      expect_equal(length(missing), min(duration, 9L - min(missing)))
    }
  }
})

test_that("long block durations clip safely at the final wave", {
  limit <- .Machine$integer.max
  for (range in list(c(8, 8), c(limit, limit), c(limit - 1L, limit),
                     rep(limit + 1, 2), rep(.Machine$double.xmax, 2))) {
    expect_no_warning(d <- generate_weasel_dummy_data(
      n_ids = 3, n_times = 8, n_vars = 1, seed = 7,
      prop_random = 0, prop_attention = 0, prop_attrition = 0,
      prop_block = 1, block_duration_range = range, prop_item_missing = 0
    ))
    expect_identical(length(unique(d$id)), 3L)
    for (times in split(d$time, d$id)) {
      expect_identical(times, seq_len(max(times)))
      expect_lt(max(times), 8L)
    }
  }
})

test_that("block sampling retains ordinary seeded ranges and schedule mapping", {
  ranges <- list(c(2L, 4L), c(4L, 2L))
  expected_missing <- list(c(2L, 4L, 2L, 2L, 4L, 2L),
                           c(4L, 2L, 4L, 4L, 2L, 4L))
  for (i in seq_along(ranges)) {
    d <- generate_weasel_dummy_data(
      n_ids = 6, n_times = 8, n_vars = 1, seed = 7,
      prop_random = 0, prop_attention = 0, prop_attrition = 0,
      prop_block = 1, block_duration_range = ranges[[i]], prop_item_missing = 0
    )
    expect_identical(8L - as.integer(table(d$id)), expected_missing[[i]])
  }
  args <- list(n_ids = 6, n_times = 8, n_vars = 2, seed = 7,
               prop_block = 1, block_duration_range = c(3, 3))
  reference <- do.call(generate_weasel_dummy_data, args)
  schedule <- seq.int(2000L, 2014L, by = 2L)
  reference$time <- schedule[reference$time]
  args$waves <- schedule
  expect_identical(do.call(generate_weasel_dummy_data, args), reference)
})

test_that("fixed block sampling is reproducible and retains caller RNG state", {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  args <- list(n_ids = 6, n_times = 8, n_vars = 2, seed = 7,
               prop_block = 1, block_duration_range = c(3, 3))
  reference <- do.call(generate_weasel_dummy_data, args)
  RNGkind("Wichmann-Hill", "Box-Muller", sample.kind = "Rejection")
  set.seed(61)
  before_seed <- .Random.seed
  before_kind <- RNGkind()
  expect_identical(do.call(generate_weasel_dummy_data, args), reference)
  expect_identical(.Random.seed, before_seed)
  expect_identical(RNGkind(), before_kind)
  args$seed <- NULL
  first <- suppressMessages(do.call(generate_weasel_dummy_data, args))
  expect_identical(suppressMessages(do.call(generate_weasel_dummy_data, args)), first)
  expect_identical(.Random.seed, before_seed)
  expect_identical(RNGkind(), before_kind)

  RNGkind("Mersenne-Twister", "Inversion", sample.kind = "Rejection")
  before_kind <- RNGkind()
  rm(".Random.seed", envir = globalenv())
  args$seed <- 7
  expect_identical(do.call(generate_weasel_dummy_data, args), reference)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_identical(RNGkind(), before_kind)
})

test_that("dummy data validates its arguments", {
  expect_error(generate_weasel_dummy_data(n_ids = 0), "n_ids")
  expect_error(generate_weasel_dummy_data(n_times = 2), "n_times")
})
