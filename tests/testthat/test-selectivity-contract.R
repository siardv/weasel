# hand-calculated covariate summaries with fixed participation groups
make_selectivity_contract_panel <- function() {
  data.frame(
    id = rep(c("retained_1", "retained_2", "excluded_1", "excluded_2"),
             c(3L, 3L, 1L, 1L)),
    time = c(1:3, 1:3, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

test_that("one constant group permits a finite selectivity SMD", {
  d <- make_selectivity_contract_panel()
  d$constant_retained <- c(rep(3, 6), 0, 2)
  d$constant_excluded <- c(rep(0, 3), rep(2, 3), 3, 3)
  p <- weasel_plan(d, "id", "time", span = "full")
  vars <- c("constant_retained", "constant_excluded")

  # one sample SD is zero, the other is sqrt(2); pooled SD is one
  for (at in c("first", "mean")) {
    s <- weasel_selectivity(p, "anchored_strict", vars = vars, at = at)
    s <- s[match(vars, s$variable), ]
    expect_identical(s$variable, vars)
    expect_equal(s$n_retained, c(2L, 2L))
    expect_equal(s$n_excluded, c(2L, 2L))
    expect_equal(s$mean_retained, c(3, 1))
    expect_equal(s$mean_excluded, c(1, 3))
    expect_equal(s$diff, c(2, -2))
    expect_equal(s$smd, c(2, -2))
  }
})

test_that("zero mean imbalance can coexist with different covariate spread", {
  d <- make_selectivity_contract_panel()
  d$x <- c(rep(-1, 3), rep(1, 3), -10, 10)
  p <- weasel_plan(d, "id", "time", span = "full")

  for (at in c("first", "mean")) {
    s <- weasel_selectivity(p, "anchored_strict", vars = "x", at = at)
    expect_equal(s$mean_retained, 0)
    expect_equal(s$mean_excluded, 0)
    expect_equal(s$diff, 0)
    expect_equal(s$smd, 0)
  }
})

test_that("zero or undefined pooled dispersion gives an unavailable SMD", {
  d <- make_selectivity_contract_panel()
  d$constant <- c(rep(3, 6), 1, 1)
  d$retained_singleton <- c(rep(0, 3), rep(NA_real_, 3), 2, 4)
  d$excluded_singleton <- c(rep(1, 3), rep(3, 3), NA_real_, 2)
  d$retained_empty <- c(rep(NA_real_, 6), 0, 2)
  d$excluded_empty <- c(rep(0, 3), rep(2, 3), NA_real_, NA_real_)
  d$both_empty <- rep(NA_real_, 8)
  p <- weasel_plan(d, "id", "time", span = "full")
  vars <- c("constant", "retained_singleton", "excluded_singleton",
            "retained_empty", "excluded_empty", "both_empty")

  for (at in c("first", "mean")) {
    s <- weasel_selectivity(p, "anchored_strict", vars = vars, at = at)
    s <- s[match(vars, s$variable), ]
    expect_identical(s$variable, vars)
    expect_equal(s$n_retained, c(2L, 1L, 2L, 0L, 2L, 0L))
    expect_equal(s$n_excluded, c(2L, 2L, 1L, 2L, 0L, 0L))
    expect_equal(s$mean_retained, c(3, 0, 2, NA_real_, 1, NA_real_))
    expect_equal(s$mean_excluded, c(1, 3, 2, 1, NA_real_, NA_real_))
    expect_equal(s$diff, c(2, -3, 0, NA_real_, NA_real_, NA_real_))
    expect_equal(s$smd, rep(NA_real_, 6))
  }
})

test_that("first selects each respondent's first observed wave even with a missing item", {
  d <- data.frame(
    id = rep(c("retained_1", "retained_2", "retained_3",
               "excluded_1", "excluded_2"), each = 2L),
    time = c(3L, 1L, 3L, 1L, 3L, 1L, 3L, 2L, 2L, 1L),
    x = c(8, NA_real_, 8, 4, 10, 6, 4, 0, 6, 2),
    stringsAsFactors = FALSE
  )
  p <- weasel_plan(d, "id", "time", span = "full")

  # first summaries are (NA, 4, 6) versus (0, 2), from different waves
  first <- weasel_selectivity(p, "anchored_balanced", vars = "x", at = "first")
  expect_equal(first$n_retained, 2L)
  expect_equal(first$n_excluded, 2L)
  expect_equal(first$mean_retained, 5)
  expect_equal(first$mean_excluded, 1)
  expect_equal(first$diff, 4)
  expect_equal(first$smd, 2 * sqrt(2))

  # mean summaries are (8, 6, 8) versus (2, 4), over observed waves
  average <- weasel_selectivity(p, "anchored_balanced", vars = "x", at = "mean")
  expect_equal(average$n_retained, 3L)
  expect_equal(average$n_excluded, 2L)
  expect_equal(average$mean_retained, 22 / 3)
  expect_equal(average$mean_excluded, 3)
  expect_equal(average$diff, 13 / 3)
  expect_equal(average$smd, 13 / sqrt(15))
})

test_that("mean gives equal weight to observed pair means after item missingness", {
  d <- data.frame(
    id = c(rep("retained_1", 6), rep("retained_2", 3),
           rep("excluded_1", 2), rep("excluded_2", 2)),
    time = c(1L, 1L, 1L, 2L, 3L, 3L, 1:3, 1L, 2L, 2L, 3L),
    x = c(0, 2, NA_real_, 5, NA_real_, NA_real_,
          2, 4, 6, 0, NA_real_, 2, NA_real_),
    stringsAsFactors = FALSE
  )
  expect_warning(p <- weasel_plan(d, "id", "time", span = "full"),
                 class = "weasel_duplicates")

  # retained_1 has pair means (1, 5, NA), not a mean of physical rows
  expected <- list(
    first = c(mean_retained = 1.5, diff = 0.5, smd = 1 / sqrt(5)),
    mean = c(mean_retained = 3.5, diff = 2.5, smd = sqrt(5))
  )
  for (at in names(expected)) {
    expect_warning(
      s <- weasel_selectivity(p, "anchored_strict", vars = "x", at = at),
      class = "weasel_duplicates"
    )
    expect_equal(s$n_retained, 2L)
    expect_equal(s$n_excluded, 2L)
    expect_equal(s$mean_retained, unname(expected[[at]]["mean_retained"]))
    expect_equal(s$mean_excluded, 1)
    expect_equal(s$diff, unname(expected[[at]]["diff"]))
    expect_equal(s$smd, unname(expected[[at]]["smd"]))
  }
})

test_that("identical copies do not create dispersion in constant groups", {
  for (excluded_value in c(0.1, 0.3)) {
    d <- make_selectivity_contract_panel()
    d$x <- c(rep(0.1, 6), excluded_value, excluded_value)
    # copy every observed pair of one respondent so mean also exposes the defect
    copied <- rbind(d, d[1:3, ], d[1:3, ])
    expect_warning(p <- weasel_plan(copied, "id", "time", span = "full"),
                   class = "weasel_duplicates")

    for (at in c("first", "mean")) {
      expect_warning(
        s <- weasel_selectivity(p, "anchored_strict", vars = "x", at = at),
        class = "weasel_duplicates"
      )
      expect_identical(s$n_retained, 2L)
      expect_identical(s$n_excluded, 2L)
      expect_identical(s$mean_retained, 0.1)
      expect_identical(s$mean_excluded, excluded_value)
      expect_identical(s$smd, NA_real_)
    }
  }
})

test_that("copies of a single-wave excluded respondent preserve both summaries", {
  d <- make_selectivity_contract_panel()
  d$x <- c(rep(0.3, 6), 0.1, 0.1)
  copied <- rbind(d, d[7, ], d[7, ])
  expect_warning(p <- weasel_plan(copied, "id", "time", span = "full"),
                 class = "weasel_duplicates")

  for (at in c("first", "mean")) {
    expect_warning(
      s <- weasel_selectivity(p, "anchored_strict", vars = "x", at = at),
      class = "weasel_duplicates"
    )
    expect_identical(s$n_retained, 2L)
    expect_identical(s$n_excluded, 2L)
    expect_identical(s$mean_retained, 0.3)
    expect_identical(s$mean_excluded, 0.1)
    expect_identical(s$smd, NA_real_)
  }
})

test_that("pair means preserve every common finite value exactly", {
  tiny <- .Machine$double.xmin * .Machine$double.eps
  values <- c(0.1, 0.3, -0.1, pi, sqrt(2), 1 + .Machine$double.eps,
              tiny, 3 * tiny, .Machine$double.xmin, .Machine$double.xmax,
              1e308, -1e308, 0, -0)
  expected <- values
  expected[expected == 0] <- 0

  for (n in 1:20) {
    pairs <- lapply(values, function(value) c(NA_real_, rep(value, n), NaN))
    x <- unlist(pairs, use.names = FALSE)
    grp <- rep(seq_along(pairs), lengths(pairs))
    expect_true(identical(weasel:::.weasel_pair_mean(x, grp), expected,
                          num.eq = FALSE), info = paste("copy count", n))
  }

  expect_true(identical(weasel:::.weasel_pair_mean(c(0, -0, NA_real_),
                                                  rep(1L, 3)),
                        0, num.eq = FALSE))
  expect_identical(weasel:::.weasel_pair_mean(c(NA_real_, NaN, NaN),
                                             c(1L, 1L, 2L)),
                   c(NA_real_, NA_real_))
  expect_identical(weasel:::.weasel_pair_mean(c(NA_real_, NaN, 0.1, 0.1,
                                               NaN, 1, 3),
                                             c(1L, 1L, 2L, 2L, 3L, 4L, 4L)),
                   c(NA_real_, 0.1, NA_real_, 2))
  expect_identical(weasel:::.weasel_pair_mean(c(1L, NA_integer_, 1L,
                                               -2L, -2L),
                                             c(1L, 1L, 1L, 2L, 2L)),
                   c(1, -2))
  expect_identical(weasel:::.weasel_pair_mean(c(TRUE, NA, TRUE, FALSE, FALSE),
                                             c(1L, 1L, 1L, 2L, 2L)),
                   c(1, 0))
})

test_that("nonconstant and infinite pairs keep their existing arithmetic", {
  pairs <- list(
    c(0.1, 0.3), c(1e16, 1, -1e16), c(1, NA_real_, 2, NaN),
    c(Inf, Inf), c(-Inf, -Inf), c(Inf, -Inf), c(Inf, 2, NA_real_),
    c(.Machine$double.xmax, -.Machine$double.xmax),
    c(.Machine$double.xmin * .Machine$double.eps, 0, 0)
  )
  x <- unlist(pairs, use.names = FALSE)
  grp <- rep(seq_along(pairs), lengths(pairs))
  # the platform's original pair operation is the compatibility oracle
  sums <- rowsum(ifelse(is.na(x), 0, x), grp)
  cnts <- rowsum(as.numeric(!is.na(x)), grp)
  expected <- as.numeric(sums / cnts)
  actual <- weasel:::.weasel_pair_mean(x, grp)
  expect_identical(is.na(actual), is.na(expected))
  expect_identical(is.nan(actual), is.nan(expected))
  expect_true(identical(actual[!is.na(expected)], expected[!is.na(expected)],
                        num.eq = FALSE))
})

test_that("original-value and missing copies preserve selectivity tables", {
  d <- make_selectivity_contract_panel()
  d$full_precision <- c(pi, sqrt(2), 0.1, -pi, 1 + .Machine$double.eps,
                        0.3, -sqrt(2), -0.1)
  d$constant_overflow <- rep(1e308, nrow(d))
  d$integer <- seq_len(nrow(d))
  d$logical <- d$integer %% 2L == 0L
  d$missing <- c(NA_real_, 2, NaN, 4, 5, 6, NA_real_, 8)
  d <- d[order(d$id, d$time), ]
  rownames(d) <- NULL
  vars <- setdiff(names(d), c("id", "time"))
  missing <- d
  for (v in vars) missing[[v]] <- NA
  copied <- rbind(d, d, d, missing)
  # accepted wave perturbations still denote the same respondent-wave pair
  copied$time[nrow(d) + seq_len(nrow(d))] <- d$time + 1e-9
  copied$time[2L * nrow(d) + seq_len(nrow(d))] <- d$time - 1e-9
  expect_no_warning(original_plan <- weasel_plan(d, "id", "time", span = "full"))
  expect_warning(copied_plan <- weasel_plan(copied, "id", "time", span = "full"),
                 class = "weasel_duplicates")
  before <- serialize(copied_plan, NULL)

  expect_identical(copied_plan$id_metrics, original_plan$id_metrics)
  expect_identical(copied_plan$plan, original_plan$plan)
  expect_identical(copied_plan$fingerprint$pair_hash,
                   original_plan$fingerprint$pair_hash)
  expect_identical(copied_plan$fingerprint$n_rows, nrow(copied))

  for (at in c("first", "mean")) {
    expect_no_warning(expected <- weasel_selectivity(
      original_plan, "anchored_strict", vars = vars, at = at
    ))
    expect_warning(actual <- weasel_selectivity(
      copied_plan, "anchored_strict", vars = vars, at = at
    ), class = "weasel_duplicates")
    expect_identical(actual, expected)
    restored <- unserialize(before)
    expect_warning(saved <- weasel_selectivity(
      restored, "anchored_strict", vars = vars, at = at
    ), class = "weasel_duplicates")
    expect_identical(saved, expected)
  }

  expect_warning(selected <- weasel_apply(copied_plan, "anchored_strict"),
                 class = "weasel_duplicates")
  expected_rows <- copied[copied$id %in% c("retained_1", "retained_2"), ]
  expect_identical(selected, expected_rows)
  expect_identical(serialize(copied_plan, NULL), before)
  expect_identical(attr(copied_plan, "data"), copied)
})

test_that("duplicate and reunion warnings keep their resolved-span boundaries", {
  d <- make_selectivity_contract_panel()
  d$x <- c(1:6, 7, 8)
  outside <- data.frame(id = "excluded_1", time = 5L, x = 9)
  d <- rbind(d, outside)
  copied <- rbind(d, outside)
  expect_no_warning(original_plan <- weasel_plan(d, "id", "time",
                                                 lower = 1, upper = 3))
  expect_warning(copied_plan <- weasel_plan(copied, "id", "time",
                                            lower = 1, upper = 3),
                 class = "weasel_duplicates")

  for (at in c("first", "mean")) {
    expect_no_warning(expected <- weasel_selectivity(
      original_plan, "anchored_strict", vars = "x", at = at
    ))
    expect_no_warning(actual <- weasel_selectivity(
      copied_plan, "anchored_strict", vars = "x", at = at
    ))
    expect_identical(actual, expected)
    expect_warning(reunited <- weasel_selectivity(
      original_plan, "anchored_strict", vars = "x", data = copied, at = at
    ), class = "weasel_data_mismatch")
    expect_identical(reunited, expected)
  }
})
