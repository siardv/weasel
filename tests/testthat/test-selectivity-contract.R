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
