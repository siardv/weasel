test_that("all styles attribute explicit windows to user-supplied bounds", {
  d <- make_fixture()
  plans <- list(
    weasel_plan(d, "id", "time", lower = 3, upper = 6),
    weasel_plan(d, "id", "time", lower = 3),
    weasel_plan(d, "id", "time", upper = 6)
  )
  observed <- d
  observed$time <- 2006L + 2L * observed$time
  plans[[4]] <- weasel_plan(observed, "id", "time", grid = "observed",
                            lower = 2009, upper = 2017)

  for (p in plans) {
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(p, "lenient", style = style, cite = FALSE)
      expect_match(txt, "user-supplied bounds", fixed = TRUE)
      expect_match(txt, sprintf("waves %s to %s", p$lower, p$upper),
                   fixed = TRUE)
      expect_false(grepl("a priori|a-priori|design decision|span rule", txt))
    }
  }
})

test_that("all styles describe full spans on the resolved wave grid", {
  d <- expand.grid(id = 1:3, time = c(1, 3, 4))
  expect_warning(
    consecutive <- weasel_plan(d, "id", "time", span = "full"),
    class = "weasel_empty_waves"
  )
  observed <- weasel_plan(d, "id", "time", span = "full", grid = "observed")
  expect_equal(consecutive$span, 1:4)
  expect_equal(observed$span, c(1L, 3L, 4L))

  for (p in list(consecutive, observed)) {
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(p, "lenient", style = style, cite = FALSE)
      expect_match(txt, "full wave grid", fixed = TRUE)
      expect_match(txt, "first to the last grid wave", fixed = TRUE)
      expect_false(grepl("stable|stability|superior|prioritizes|maximiz", txt))
    }
  }
})

test_that("all styles describe the core coverage objective and tie policy", {
  # the high-coverage window includes different respondents at each wave
  d <- rbind(data.frame(id = "anchor", time = 1:6),
             data.frame(id = paste0("extra", 1:6), time = rep(4:6, each = 2)))
  consecutive <- weasel_plan(d, "id", "time", span = "core", core_len = 3)
  expect_equal(consecutive$span, 4:6)
  expect_equal(max(consecutive$span_candidates$coverage), 9L)

  observed <- d
  observed$time <- c(2008, 2010, 2014, 2016, 2020, 2022)[observed$time]
  observed <- weasel_plan(observed, "id", "time", span = "core",
                          core_len = 3, grid = "observed")
  expect_equal(observed$span, c(2016L, 2020L, 2022L))

  expect_warning(
    tied <- weasel_plan(expand.grid(id = 1:3, time = 1:6), "id", "time",
                        span = "core", core_len = 3),
    class = "weasel_tied_windows"
  )
  expect_equal(tied$span, 1:3)

  # an oversized core request still records the core rule, with one candidate
  clamped <- weasel_plan(d, "id", "time", span = "core", core_len = 20)
  expect_identical(clamped$span_reason, "core")
  expect_equal(clamped$span, 1:6)
  expect_equal(nrow(clamped$span_candidates), 1L)

  for (p in list(consecutive, observed, tied, clamped)) {
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(p, "lenient", style = style, cite = FALSE)
      expect_match(txt, "core", fixed = TRUE)
      expect_match(txt, "highest total count of distinct respondent-wave pairs",
                   fixed = TRUE)
      expect_match(txt, "contiguous windows of the same length on the wave grid",
                   fixed = TRUE)
      expect_match(txt, "ties are resolved in favour of the earliest window",
                   fixed = TRUE)
      expect_false(grepl("stable|stability|superior|comparatively strong|shorter", txt))
    }
  }
})

test_that("window descriptions preserve missing and unknown reason fallbacks", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  for (reason in list(NULL, NA_character_, "")) {
    legacy <- p
    legacy$plan$span_reason <- reason
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(legacy, "lenient", style = style, cite = FALSE)
      expect_match(txt, "waves 1 to 8", fixed = TRUE)
      expect_false(grepl("full wave grid|span rule|user-supplied bounds", txt))
    }
  }

  unknown <- p
  unknown$plan$span_reason <- "legacy_rule"
  for (style in c("methods", "concise", "extended")) {
    txt <- weasel_justify_subset(unknown, "lenient", style = style, cite = FALSE)
    expect_match(txt, "span rule (legacy_rule)", fixed = TRUE)
    expect_false(grepl("stable|stability|superior|prioritizes|full wave grid", txt))
  }
})

test_that("window explanations work with saved-plan metadata and leave selection intact", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "core", core_len = 8,
                   keep_data = FALSE)
  legacy <- p
  legacy$span_candidates <- NULL
  legacy$grid <- NULL
  legacy$span <- NULL
  names(legacy$plan)[names(legacy$plan) == "max_gap_len"] <- "max_gap_max"
  selected <- weasel_apply(p, "lenient", data = d)
  before <- serialize(legacy, NULL)

  for (style in c("methods", "concise", "extended")) {
    expect_identical(
      weasel_justify_subset(legacy, "lenient", style = style, cite = FALSE),
      weasel_justify_subset(p, "lenient", style = style, cite = FALSE)
    )
  }
  expect_identical(serialize(legacy, NULL), before)
  expect_identical(weasel_apply(legacy, "lenient", data = d), selected)
})
