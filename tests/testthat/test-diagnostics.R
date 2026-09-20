test_that("sensitivity sweep is complete, bounded, and monotone", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  s <- weasel_sensitivity(p, require_endpoints = c(TRUE, FALSE),
                          max_missing = 0:3, n_gap_max = 0:2,
                          max_gap_len = 0:2)

  expect_equal(nrow(s), 2 * 4 * 3 * 3)
  expect_named(s, c("require_endpoints", "max_missing", "n_gap_max",
                    "max_gap_len", "n_ids", "prop_ids",
                    "mean_prop_present"))
  n_total <- nrow(p$id_metrics)
  expect_true(all(s$n_ids >= 0 & s$n_ids <= n_total))
  expect_equal(s$prop_ids, s$n_ids / n_total)

  # loosening the missingness tolerance never shrinks the sample
  base <- s[!s$require_endpoints & s$n_gap_max == 2 & s$max_gap_len == 2, ]
  base <- base[order(base$max_missing), ]
  expect_true(all(diff(base$n_ids) >= 0))

  # requiring endpoints can only reduce the sample
  key <- paste(s$max_missing, s$n_gap_max, s$max_gap_len)
  a <- s[s$require_endpoints, ]
  f <- s[!s$require_endpoints, ]
  a <- a[order(paste(a$max_missing, a$n_gap_max, a$max_gap_len)), ]
  f <- f[order(paste(f$max_missing, f$n_gap_max, f$max_gap_len)), ]
  expect_true(all(a$n_ids <= f$n_ids))

  # the plan's own scenarios are reproduced by matching combinations
  bal <- s[s$require_endpoints & s$max_missing == 1 &
             s$n_gap_max == 1 & s$max_gap_len == 1, ]
  expect_equal(bal$n_ids,
               p$plan$n_ids[p$plan$scenario == "anchored_balanced"])

  expect_error(weasel_sensitivity(p, max_missing = -1), "non-negative")
  expect_error(weasel_sensitivity(list()), class = "weasel_error_plan")
})

test_that("sensitivity preserves valid integer boundaries and normalization", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full",
                    keep_data = FALSE)
  before <- serialize(p, NULL)
  limit <- .Machine$integer.max
  zero_counts <- c(max_missing = 2L, n_gap_max = 4L, max_gap_len = 4L)
  for (nm in names(zero_counts)) {
    args <- list(plan_obj = p, require_endpoints = FALSE,
                 max_missing = 8L, n_gap_max = 8L, max_gap_len = 8L)
    for (value in list(limit, as.double(limit))) {
      args[[nm]] <- value
      expect_no_warning(s <- do.call(weasel_sensitivity, args))
      expect_identical(s[[nm]], limit)
      expect_identical(s$n_ids, 7L)
      expect_identical(s$prop_ids, 1)
      expect_identical(s$mean_prop_present, 0.75)
    }
    args[[nm]] <- c(limit, 0, limit)
    expect_no_warning(s <- do.call(weasel_sensitivity, args))
    expect_identical(s[[nm]], c(0L, limit))
    expect_identical(s$n_ids, c(unname(zero_counts[nm]), 7L))
    expect_identical(s$prop_ids, s$n_ids / 7)
  }

  expected <- weasel_sensitivity(p, max_missing = 0:2, n_gap_max = 0:2,
                                 max_gap_len = 0:2)
  expect_no_warning(actual <- weasel_sensitivity(p,
    require_endpoints = c(FALSE, TRUE, FALSE),
    max_missing = c(2, 1 + 1e-9, 0, 1 - 1e-9),
    n_gap_max = c(2, 0, 1, 2), max_gap_len = c(1, 2, 0, 1)
  ))
  expect_identical(actual, expected)
  expect_identical(vapply(actual, typeof, character(1)),
    c(require_endpoints = "logical", max_missing = "integer",
      n_gap_max = "integer", max_gap_len = "integer", n_ids = "integer",
      prop_ids = "double", mean_prop_present = "double"))
  expect_identical(serialize(p, NULL), before)
})

test_that("sensitivity rejects overflowing vectors before integer conversion", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  before <- serialize(p, NULL)
  limit <- .Machine$integer.max
  for (nm in c("max_missing", "n_gap_max", "max_gap_len")) {
    for (value in list(limit + 1, c(0, limit + 1), c(limit + 1, limit),
                       c(limit + 1, limit + 2), .Machine$double.xmax)) {
      args <- list(plan_obj = p)
      args[[nm]] <- value
      expect_no_warning(expect_error(
        do.call(weasel_sensitivity, args),
        paste0(nm, ".*between 0 and 2147483647"), class = "weasel_error"
      ))
    }
  }
  expect_identical(serialize(p, NULL), before)
})

test_that("sensitivity retains earlier tolerance validation and argument order", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  overflow <- .Machine$integer.max + 1
  for (nm in c("max_missing", "n_gap_max", "max_gap_len")) {
    for (value in list(NULL, numeric(), TRUE, "1", factor("1"), list(1),
                       1 + 0i, NA_real_, NaN, Inf, -Inf, -1e-9, 1.5,
                       1.1e-8, c(overflow, NA), c(overflow, Inf),
                       c(overflow, -1), c(overflow, 1.5))) {
      args <- list(plan_obj = p)
      args[nm] <- list(value)
      expect_no_warning(expect_error(
        do.call(weasel_sensitivity, args),
        paste0(nm, " must be non-negative integers"), class = "weasel_error"
      ))
    }
  }
  expect_no_warning(expect_error(
    weasel_sensitivity(list(), max_missing = overflow),
    class = "weasel_error_plan"
  ))
  expect_no_warning(expect_error(
    weasel_sensitivity(p, require_endpoints = 1, max_missing = overflow),
    "require_endpoints", class = "weasel_error"
  ))
  expect_no_warning(expect_error(
    weasel_sensitivity(p, max_missing = -1, n_gap_max = overflow),
    "max_missing.*non-negative", class = "weasel_error"
  ))
  expect_no_warning(expect_error(
    weasel_sensitivity(p, n_gap_max = -1, max_gap_len = overflow),
    "n_gap_max.*non-negative", class = "weasel_error"
  ))
  expect_no_warning(expect_error(
    weasel_sensitivity(p, max_missing = overflow, n_gap_max = -1),
    "max_missing.*between", class = "weasel_error"
  ))
})

test_that("sensitivity alias forwarding and explicit precedence retain their rules", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  limit <- .Machine$integer.max
  expected <- weasel_sensitivity(p, max_gap_len = c(0, limit))
  expect_no_warning(expect_warning(
    actual <- weasel_sensitivity(p, max_gap_max = c(limit, 0, limit)),
    class = "weasel_deprecated"
  ))
  expect_identical(actual, expected)
  for (value in list(limit + 1, c(0, limit + 1))) {
    expect_no_warning(expect_warning(expect_error(
      weasel_sensitivity(p, max_gap_max = value),
      "max_gap_len.*between 0 and 2147483647", class = "weasel_error"
    ), class = "weasel_deprecated"))
    expect_no_warning(expect_warning(
      actual <- weasel_sensitivity(p, max_gap_len = c(0, limit),
                                   max_gap_max = value),
      class = "weasel_deprecated"
    ))
    expect_identical(actual, expected)
  }
  expect_no_warning(expect_warning(expect_error(
    weasel_sensitivity(p, max_gap_len = NULL, max_gap_max = limit + 1),
    "max_gap_len.*non-negative", class = "weasel_error"
  ), class = "weasel_deprecated"))
  expect_no_warning(expect_warning(expect_error(
    weasel_sensitivity(p, require_endpoints = 1, max_gap_max = limit + 1),
    "require_endpoints", class = "weasel_error"
  ), class = "weasel_deprecated"))
})

test_that("scenario tables retain separate unlimited and large finite tolerances", {
  values <- c(Inf, .Machine$integer.max + 1)
  scenarios <- data.frame(scenario = c("unlimited", "large"),
                          require_endpoints = FALSE, max_missing = values,
                          n_gap_max = values, max_gap_len = values)
  expect_no_warning(p <- weasel_plan(make_fixture(), "id", "time",
                                     span = "full", scenarios = scenarios))
  for (nm in c("max_missing", "n_gap_max", "max_gap_len")) {
    expect_identical(p$plan[[nm]], values)
  }
  expect_identical(p$plan$n_ids, c(7, 7))
  expect_identical(p$plan$ids[[1]], p$plan$ids[[2]])
})

test_that("selectivity flags a covariate that drives exclusion", {
  # ids 1..20 complete; ids 21..40 miss the last wave; x differs by group
  full <- expand.grid(id = 1:40, time = 1:6)
  full <- full[!(full$id > 20 & full$time == 6), ]
  full$x <- ifelse(full$id > 20, 11, 1)  # excluded group much higher
  full$z <- 1                            # identical in both groups
  p <- weasel_plan(full, "id", "time", span = "full")

  sel <- weasel_selectivity(p, "anchored_strict")
  expect_named(sel, c("variable", "n_retained", "n_excluded",
                      "mean_retained", "mean_excluded", "diff", "smd"))
  expect_equal(sel$variable[1], "x")

  x_row <- sel[sel$variable == "x", ]
  expect_equal(x_row$n_retained, 20L)
  expect_equal(x_row$n_excluded, 20L)
  expect_equal(x_row$mean_retained, 1)
  expect_equal(x_row$mean_excluded, 11)
  expect_equal(x_row$diff, -10)

  # zero spread in both groups: smd is NA, sorted last
  z_row <- sel[sel$variable == "z", ]
  expect_true(is.na(z_row$smd))

  # explicit vars and at = "mean"
  sel2 <- weasel_selectivity(p, "anchored_strict", vars = "x", at = "mean")
  expect_equal(nrow(sel2), 1L)
  expect_equal(sel2$diff, -10)

  # validation
  expect_error(weasel_selectivity(p, "anchored_strict", vars = "nope"),
               "not found")
  full$chr <- "a"
  p2 <- weasel_plan(full, "id", "time", span = "full")
  sel3 <- weasel_selectivity(p2, "anchored_strict")
  expect_false("chr" %in% sel3$variable)
  expect_error(weasel_selectivity(p2, "anchored_strict", vars = "chr"),
               "numeric")

  # everyone retained: nothing to compare against
  dd <- expand.grid(id = 1:5, time = 1:4)
  dd$x <- 1
  p3 <- weasel_plan(dd, "id", "time", span = "full")
  expect_error(weasel_selectivity(p3, "lenient"), "excluded")
})

test_that("diagnostics work without attached data where possible", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full", keep_data = FALSE)

  s <- weasel_sensitivity(p, max_missing = 0:1, n_gap_max = 0:1,
                          max_gap_len = 0:1)
  expect_gt(nrow(s), 0)

  expect_error(weasel_selectivity(p, "lenient"), "keep_data")
  sel <- weasel_selectivity(p, "lenient", data = d)
  expect_s3_class(sel, "data.frame")
  expect_true("var1" %in% sel$variable)
})
