# regression tests for the dirty-input and degenerate-case contracts
# introduced in the 0.4.0 cycle (stage 3)

test_that("compare sentence never fabricates a recommendation", {
  d <- make_fixture()
  imp <- data.frame(
    scenario = c("s1", "s2"), require_endpoints = TRUE,
    max_missing = 0, n_gap_max = 0, max_gap_len = 0
  )
  d2 <- d[d$id %in% c("d1", "f1"), ]  # nobody has both endpoints
  p <- suppressMessages(
    weasel_plan(d2, "id", "time", span = "full", scenarios = imp)
  )
  cmp <- weasel_compare_scenarios(p)
  expect_false(any(cmp$recommended))
  s <- weasel_compare_to_sentence(cmp)
  expect_false(grepl("Recommended scenario:", s, fixed = TRUE))
  expect_match(s, "No scenario is recommended")
})

test_that("selectivity is deterministic and pair-counting under duplicates", {
  full <- expand.grid(id = 1:40, time = 1:6)
  full <- full[!(full$id > 20 & full$time == 6), ]
  full$x <- ifelse(full$id > 20, 11, 1)
  # a conflicting duplicate row for (id 1, wave 1)
  dup <- rbind(full, transform(full[full$id == 1 & full$time == 1, ], x = 999))

  p_fwd <- suppressWarnings(weasel_plan(dup, "id", "time", span = "full"))
  p_rev <- suppressWarnings(
    weasel_plan(dup[rev(seq_len(nrow(dup))), ], "id", "time", span = "full")
  )

  expect_warning(
    s_fwd <- weasel_selectivity(p_fwd, "anchored_strict", vars = "x"),
    class = "weasel_duplicates"
  )
  s_rev <- suppressWarnings(
    weasel_selectivity(p_rev, "anchored_strict", vars = "x")
  )
  # row order no longer changes the diagnostic
  expect_equal(s_fwd, s_rev)
  # the duplicated pair enters once, as the within-pair average (500)
  expect_equal(s_fwd$mean_retained, mean(c(500, rep(1, 19))))

  # at = "mean" counts the duplicated pair once as well
  expect_warning(
    s_mean <- weasel_selectivity(p_fwd, "anchored_strict", vars = "x",
                                 at = "mean"),
    class = "weasel_duplicates"
  )
  s_mean_rev <- suppressWarnings(
    weasel_selectivity(p_rev, "anchored_strict", vars = "x", at = "mean")
  )
  expect_equal(s_mean, s_mean_rev)
})

test_that("apply and summarize warn when output rows contain duplicates", {
  d <- make_fixture()
  dd <- rbind(d, d[d$id == "a1", ][1:3, ])
  p <- suppressWarnings(weasel_plan(dd, "id", "time", span = "full"))

  expect_warning(sub <- weasel_apply(p, "lenient"),
                 class = "weasel_duplicates")
  expect_gt(sum(duplicated(sub[c("id", "time")])), 0)

  expect_warning(weasel_summarize_subset(p, "lenient"),
                 class = "weasel_duplicates")

  # clean data stays silent
  p2 <- weasel_plan(d, "id", "time", span = "full")
  expect_no_warning(weasel_apply(p2, "lenient"))
  expect_no_warning(weasel_summarize_subset(p2, "lenient"))
})

test_that("id and wave must be different columns", {
  d <- make_fixture()
  expect_error(weasel_plan(d, "time", "time", span = "full"),
               "different columns")
  expect_error(set_weasel_scope(d, "id", "id"), "different columns")
})

test_that("dropped rows are accounted for in a verbose message", {
  d <- make_fixture()
  d$id[1:2] <- NA
  old <- options(weasel.verbose = TRUE)
  on.exit(options(old), add = TRUE)
  expect_message(weasel_plan(d, "id", "time", span = "full"),
                 "2 with missing id")
})

test_that("justification refuses scenarios that retain nobody", {
  d <- make_fixture()
  imp <- data.frame(
    scenario = "impossible", require_endpoints = TRUE,
    max_missing = 0, n_gap_max = 0, max_gap_len = 0
  )
  d2 <- d[d$id %in% c("d1", "f1"), ]
  p <- suppressMessages(
    weasel_plan(d2, "id", "time", span = "full", scenarios = imp)
  )
  expect_error(weasel_justify_subset(p, "impossible"),
               class = "weasel_error_empty_scenario")
  expect_error(weasel_summarize_subset(p, "impossible"),
               class = "weasel_error_empty_scenario")
})

test_that("scenario matching uses prefixes, not substrings", {
  choices <- c("anchored_strict", "anchored_balanced", "lenient_info_max")
  expect_equal(weasel_match_scenario("anchored_b", choices),
               "anchored_balanced")
  expect_error(weasel_match_scenario("ed_ba", choices),
               class = "weasel_error_scenario")
  expect_error(weasel_match_scenario("strict", choices),
               class = "weasel_error_scenario")
})

test_that("fractional constraint values are rejected, never truncated", {
  d <- make_fixture()
  expect_error(set_weasel_scope(d, "id", "time", max_gap_len = 1.9), "fractional")
  expect_error(set_weasel_scope(d, "id", "time", lower = 2.6), "fractional")
  expect_error(set_weasel_scope(d, "id", "time", min_present = 3.5),
               "fractional")
  expect_error(weasel_plan(d, "id", "time", core_len = 6.5), "fractional")

  p <- weasel_plan(d, "id", "time", span = "full")
  expect_error(weasel_sensitivity(p, max_missing = 1.9), "fractional")

  sc <- data.frame(scenario = "s", require_endpoints = FALSE,
                   max_missing = 1.5, n_gap_max = 8, max_gap_len = 8)
  expect_error(weasel_plan(d, "id", "time", span = "full", scenarios = sc),
               "fractional")

  # Inf remains a valid "no constraint" tolerance in scenario tables
  sc_inf <- data.frame(scenario = "s", require_endpoints = FALSE,
                       max_missing = Inf, n_gap_max = Inf, max_gap_len = Inf)
  p_inf <- weasel_plan(d, "id", "time", span = "full", scenarios = sc_inf)
  expect_equal(p_inf$plan$n_ids, 7L)

  expect_error(weasel_print_table(data.frame(a = 1), digits = 2.5),
               "fractional")
  expect_error(generate_weasel_dummy_data(n_ids = 10.5), "fractional")
  expect_error(generate_weasel_dummy_data(waves = c(2008.5, 2010, 2012)),
               "fractional")
})

test_that("integer-valued doubles are still accepted everywhere", {
  d <- make_fixture()
  expect_no_error(set_weasel_scope(d, "id", "time", max_gap_len = 2,
                                   lower = 1, upper = 8, min_present = 3))
  weasel_clear_scope()
  p <- weasel_plan(d, "id", "time", span = "core", core_len = 4)
  expect_s3_class(p, "weasel_plan")
  s <- weasel_sensitivity(p, max_missing = c(0, 1, 2))
  expect_gt(nrow(s), 0)
})
