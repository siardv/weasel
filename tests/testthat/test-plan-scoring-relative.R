# scoring contract: formula, comparison-relative size, ties and empty scenarios

default_rules <- function() {
  data.frame(
    scenario = c("anchored_strict", "anchored_balanced", "lenient_info_max"),
    require_endpoints = c(TRUE, TRUE, FALSE),
    max_missing = c(0, 1, 2), n_gap_max = c(0, 1, 2), max_gap_len = c(0, 1, 2)
  )
}

test_that("default scores on the fixture equal the documented formula", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  cmp <- weasel_compare_scenarios(p)
  # hand-worked from the fixture patterns (L = 8):
  # anchored_strict  : a1, a2          -> 2*1 + 1.2*1 + 0.8*(2/5)
  # anchored_balanced: a1, a2, b1      -> 2*(23/24) + 1.2 + 0.8*(3/5)
  #                                       - 0.6*(1/8) - 0.4*((1/3 + 1/3)/8)
  # lenient_info_max : a1, a2, b1, c1, d1 -> 2*(35/40) + 1.2*(4/5) + 0.8
  #                                       - 0.6*(2/8) - 0.4*((2/5 + 3/5)/8)
  expected <- c(
    anchored_strict   = 2 + 1.2 + 0.8 * 2 / 5,
    anchored_balanced = 2 * 23 / 24 + 1.2 + 0.8 * 3 / 5 - 0.6 / 8 - 0.4 * (2 / 3) / 8,
    lenient_info_max  = 2 * 35 / 40 + 1.2 * 4 / 5 + 0.8 - 0.6 * 2 / 8 - 0.4 * 1 / 8
  )
  expect_equal(setNames(cmp$score, cmp$scenario), expected)
  expect_identical(cmp$scenario[cmp$recommended], "anchored_strict")
})

test_that("scores are comparison-relative through the size term only", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  three <- weasel_compare_scenarios(p)
  two <- weasel_compare_scenarios(
    list(plan = p$plan[p$plan$scenario != "lenient_info_max", ])
  )
  # the two anchored scenarios retain identical subsets in both
  # comparisons, yet the recommendation moves with the comparison set
  expect_identical(two$ids, three$ids[1:2])
  expect_identical(three$scenario[three$recommended], "anchored_strict")
  expect_identical(two$scenario[two$recommended], "anchored_balanced")

  # every term except size is unchanged; size is rescaled by max(n_ids)
  c3 <- attr(three, "score_components")[1:2, ]
  c2 <- attr(two, "score_components")
  fixed <- c("coverage", "endpoints", "missing", "gaps")
  expect_equal(c2[fixed], c3[fixed], ignore_attr = TRUE)
  expect_equal(c2$size, 0.8 * two$n_ids / max(two$n_ids))
  expect_equal(c3$size, 0.8 * three$n_ids[1:2] / max(three$n_ids))

  # a comparator that does not exceed the current largest n_ids leaves
  # existing scores untouched; a larger one changes all of them
  base <- default_rules()
  smaller <- rbind(base, data.frame(scenario = "smaller", require_endpoints = TRUE,
                                    max_missing = 2, n_gap_max = 1, max_gap_len = 2))
  larger <- rbind(base, data.frame(scenario = "larger", require_endpoints = FALSE,
                                   max_missing = Inf, n_gap_max = Inf, max_gap_len = Inf))
  cs <- weasel_compare_scenarios(weasel_plan(d, "id", "time", span = "full",
                                             scenarios = smaller))
  cl <- weasel_compare_scenarios(weasel_plan(d, "id", "time", span = "full",
                                             scenarios = larger))
  expect_lt(cs$n_ids[4], max(cs$n_ids[1:3]))
  expect_identical(cs$score[1:3], three$score)
  expect_gt(cl$n_ids[4], max(cl$n_ids[1:3]))
  expect_false(any(cl$score[1:3] == three$score))
  expect_equal(cl$score[1:3] - three$score,
               0.8 * three$n_ids * (1 / cl$n_ids[4] - 1 / max(three$n_ids)))

  # an unchanged maximum leaves existing scores unchanged, yet the added
  # scenario can itself become the recommended one
  pair <- weasel_compare_scenarios(list(plan = p$plan[2:3, ]))
  expect_identical(pair$scenario[pair$recommended], "anchored_balanced")
  expect_identical(pair$score, three$score[2:3])
  expect_lte(three$n_ids[1], max(pair$n_ids))
  expect_identical(three$scenario[three$recommended], "anchored_strict")
})

test_that("a zero size weight makes scores independent of the comparison set", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  three <- weasel_compare_scenarios(p)
  three0 <- weasel_compare_scenarios(p, weights = c(size = 0))
  two0 <- weasel_compare_scenarios(
    list(plan = p$plan[p$plan$scenario != "lenient_info_max", ]),
    weights = c(size = 0)
  )
  expect_equal(three0$score, three$score - attr(three, "score_components")$size)
  expect_identical(two0$score, three0$score[1:2])
  expect_identical(three0$scenario[three0$recommended], "anchored_strict")
  expect_identical(two0$scenario[two0$recommended], "anchored_strict")
})

test_that("empty scenarios keep NA scores in every comparison", {
  d <- make_fixture()
  d2 <- d[d$id %in% c("d1", "f1"), ]  # nobody has both endpoints
  rules <- data.frame(
    scenario = c("impossible", "one_missing", "unlimited"),
    require_endpoints = c(TRUE, FALSE, FALSE),
    max_missing = c(0, 1, Inf), n_gap_max = c(0, 1, Inf), max_gap_len = c(0, 1, Inf)
  )
  p <- weasel_plan(d2, "id", "time", span = "full", scenarios = rules)
  # one_missing retains d1 only; unlimited retains both, so adding it
  # changes the largest n_ids
  expect_equal(p$plan$n_ids, c(0, 1, 2))
  with_max <- weasel_compare_scenarios(p)
  without_max <- weasel_compare_scenarios(list(plan = p$plan[1:2, ]))
  zero_size <- weasel_compare_scenarios(p, weights = c(size = 0))
  for (cmp in list(with_max, without_max, zero_size)) {
    expect_true(is.na(cmp$score[cmp$scenario == "impossible"]))
    expect_false(cmp$recommended[cmp$scenario == "impossible"])
    expect_false(cmp$near_tie[cmp$scenario == "impossible"])
    expect_identical(sum(cmp$recommended), 1L)
  }
  expect_equal(with_max$score[2] - without_max$score[2], 0.8 * 1 * (1 / 2 - 1 / 1))
  expect_equal(zero_size$score[2],
               with_max$score[2] - attr(with_max, "score_components")$size[2])
})

test_that("exact score ties go to the larger sample, then to table order", {
  d <- make_fixture()
  # x retains a1, a2; y retains a1, a2, b1, c1; both score exactly 3.6
  xy <- data.frame(scenario = c("x_n2", "y_n4"), require_endpoints = TRUE,
                   max_missing = c(0, 2), n_gap_max = c(0, 1), max_gap_len = c(0, 2))
  for (tab in list(xy, xy[2:1, ])) {
    cmp <- weasel_compare_scenarios(
      weasel_plan(d, "id", "time", span = "full", scenarios = tab)
    )
    expect_true(cmp$score[1] == cmp$score[2])
    expect_identical(cmp$scenario[cmp$recommended], "y_n4")
    expect_true(all(cmp$near_tie))
  }
  # identical subsets: equal score and n_ids, the first row is recommended
  twins <- data.frame(scenario = c("twin_a", "twin_b"), require_endpoints = FALSE,
                      max_missing = 2, n_gap_max = 2, max_gap_len = 2)
  for (tab in list(twins, twins[2:1, ])) {
    cmp <- weasel_compare_scenarios(
      weasel_plan(d, "id", "time", span = "full", scenarios = tab)
    )
    expect_identical(cmp$scenario[cmp$recommended], tab$scenario[1])
    expect_true(all(cmp$near_tie))
  }
})

test_that("near ties are flagged without changing the recommendation", {
  d <- make_fixture()
  base <- default_rules()
  # anchored_strict (2 respondents, 3.520) leads anchored_balanced
  # (3 respondents, 3.488) by about 0.032: outside the default tolerance,
  # inside 0.04, in either input order
  for (tab in list(base, base[c(2, 1, 3), ])) {
    p <- weasel_plan(d, "id", "time", span = "full", scenarios = tab)
    default <- weasel_compare_scenarios(p)
    wide <- weasel_compare_scenarios(p, tie_tolerance = 0.04)
    strict <- wide$scenario == "anchored_strict"
    balanced <- wide$scenario == "anchored_balanced"
    gap <- wide$score[strict] - wide$score[balanced]
    expect_true(gap > 0.01 && gap <= 0.04)
    expect_lt(wide$n_ids[strict], wide$n_ids[balanced])
    expect_identical(wide$near_tie, strict | balanced)
    expect_false(any(default$near_tie))
    expect_identical(wide$scenario[wide$recommended], "anchored_strict")
    expect_identical(wide$recommended, default$recommended)
    expect_identical(wide$score, default$score)
    expect_match(weasel_compare_to_sentence(wide), "not unique", fixed = TRUE)
    expect_false(grepl("not unique", weasel_compare_to_sentence(default), fixed = TRUE))
  }
})
