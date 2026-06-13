test_that("character ids survive the whole plan pipeline", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")

  expect_true(all(p$plan$n_ids >= 0))
  expect_gt(sum(p$plan$n_ids), 0)

  # lenient_info_max must keep ids and weasel_apply must return their rows
  row <- p$plan[p$plan$scenario == "lenient_info_max", ]
  expect_gt(row$n_ids, 0)
  expect_type(row$ids[[1]], "character")

  sub <- weasel_apply(p, "lenient")
  expect_gt(nrow(sub), 0)
  expect_true(all(sub$id %in% row$ids[[1]]))
  # claimed n_ids matches what apply() actually returns
  expect_equal(length(unique(sub$id)), row$n_ids)
})

test_that("plan metrics use interior gaps and endpoint flags", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  m <- p$id_metrics

  expect_equal(m$n_gap[m$id == "b1"], 1L)
  expect_equal(m$max_gap[m$id == "c1"], 2L)
  expect_equal(m$n_gap[m$id == "e1"], 3L)
  # d1 misses both endpoints but has no interior gap
  expect_equal(m$n_gap[m$id == "d1"], 0L)
  expect_false(m$has_lower[m$id == "d1"])
  expect_false(m$has_upper[m$id == "d1"])
})

test_that("custom scenario tables are validated", {
  d <- make_fixture()
  bad <- data.frame(scenario = "x", require_endpoints = TRUE,
                    max_missing = 1)  # missing n_gap_max, max_gap_max
  expect_error(weasel_plan(d, "id", "time", scenarios = bad),
               "missing required column")

  dup <- data.frame(
    scenario = c("x", "x"), require_endpoints = c(TRUE, FALSE),
    max_missing = c(0, 1), n_gap_max = c(0, 1), max_gap_max = c(0, 1)
  )
  expect_error(weasel_plan(d, "id", "time", scenarios = dup), "unique")

  neg <- data.frame(
    scenario = "x", require_endpoints = TRUE,
    max_missing = -1, n_gap_max = 0, max_gap_max = 0
  )
  expect_error(weasel_plan(d, "id", "time", scenarios = neg), ">= 0")
})

test_that("scores are computed from observed outcomes with documented weights", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  cmp <- weasel_compare_scenarios(p)

  expect_true(all(c("score", "recommended") %in% names(cmp)))
  expect_equal(sum(cmp$recommended), 1)
  expect_false(any(cmp$recommended & cmp$n_ids == 0))

  # weights are overridable and validated
  cmp2 <- weasel_compare_scenarios(p, weights = c(size = 50))
  expect_equal(cmp2$scenario[cmp2$recommended],
               cmp2$scenario[which.max(cmp2$n_ids)])
  expect_error(weasel_compare_scenarios(p, weights = c(banana = 1)),
               "named vector")
})

test_that("empty scenarios get NA scores and are never recommended", {
  d <- make_fixture()
  # "impossible" requires both endpoints; the reduced data below has none
  imp <- data.frame(
    scenario = c("impossible", "lenient"),
    require_endpoints = c(TRUE, FALSE),
    max_missing = c(0, 8), n_gap_max = c(0, 8), max_gap_max = c(0, 8)
  )
  d2 <- d[d$id %in% c("d1", "f1"), ]  # nobody has both endpoints
  p <- weasel_plan(d2, "id", "time", span = "full", scenarios = imp)
  cmp <- weasel_compare_scenarios(p)
  expect_true(is.na(cmp$score[cmp$scenario == "impossible"]))
  expect_false(cmp$recommended[cmp$scenario == "impossible"])
  expect_true(cmp$recommended[cmp$scenario == "lenient"])
})

test_that("core span selection returns contiguous integer windows", {
  # waves 3 and 4 unobserved: a naive top-L pick would bridge the hole
  d <- expand.grid(id = 1:10, time = c(1, 2, 5, 6, 7, 8))
  p <- weasel_plan(d, "id", "time", span = "core", core_len = 4)
  expect_equal(p$upper - p$lower + 1L, 4L)
  # the best contiguous 4-wave window of observed coverage is 5:8
  expect_equal(p$lower, 5L)
  expect_equal(p$upper, 8L)
})

test_that("summarize_subset defaults to the data stored in the plan", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")

  s <- weasel_summarize_subset(p, "lenient_info_max")
  expect_named(
    s, c("headline", "per_wave_coverage", "missing_distribution",
         "data", "sentence")
  )
  expect_equal(s$headline$n_ids,
               p$plan$n_ids[p$plan$scenario == "lenient_info_max"])
  expect_equal(nrow(s$per_wave_coverage), p$upper - p$lower + 1L)
  expect_match(s$sentence, "respondent")

  # explicit data still works (back-compatible signature)
  s2 <- weasel_summarize_subset(p, "lenient_info_max", data = d,
                                id = "id", wave = "time")
  expect_equal(s2$headline, s$headline)

  # a scenario retaining nobody errors clearly
  d2 <- d[d$id %in% c("d1", "f1"), ]
  p2 <- weasel_plan(d2, "id", "time", span = "full")
  expect_error(weasel_summarize_subset(p2, "anchored_strict"),
               "retains no respondents")
})

test_that("worst_missing reflects observed missingness", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  len <- p$plan[p$plan$scenario == "lenient_info_max", ]
  # f1 (3 of 8 waves) is retained by lenient settings? n_missing = 5 > max_missing = 2,
  # so the worst retained respondent has at most 2 missing waves
  expect_lte(len$worst_missing, 2)
  expect_gte(len$worst_missing, 0)
})
