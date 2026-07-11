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
