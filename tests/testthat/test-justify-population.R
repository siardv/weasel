test_that("all styles distinguish planning population from supplied and retained ids", {
  d <- make_fixture()
  observed <- d
  observed$time <- c(2008L, 2010L, 2014L, 2016L, 2020L, 2022L,
                     2026L, 2030L)[observed$time]
  plans <- list(
    weasel_plan(d, "id", "time", lower = 4, upper = 6),
    weasel_plan(observed, "id", "time", grid = "observed",
                lower = 2016, upper = 2022)
  )
  population_sentence <- paste0(
    "The planning population comprised 6 respondent(s) observed at least ",
    "once within the analysis window, out of 7 distinct respondent(s) in ",
    "the supplied data; retention figures are relative to this in-window population."
  )

  for (p in plans) {
    expect_equal(p$population$n_ids_data, 7L)
    expect_equal(p$population$n_ids_in_span, 6L)
    expect_equal(p$plan$n_ids[p$plan$scenario == "anchored_strict"], 4L)
    selected <- weasel_apply(p, "anchored_strict")
    expect_identical(unique(selected$id), c("a1", "a2", "b1", "d1"))
    before <- serialize(p, NULL)

    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(p, "anchored_strict", style = style,
                                   cite = FALSE)
      expect_match(txt, population_sentence, fixed = TRUE)
      expect_match(txt, "retained 4 respondent\\(s\\)|4 respondent\\(s\\) were retained")
    }

    expect_identical(serialize(p, NULL), before)
    expect_identical(weasel_apply(p, "anchored_strict"), selected)
  }
})

test_that("saved plans report recorded populations without attached data", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", lower = 4, upper = 6,
                   keep_data = FALSE)
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(p, f)
  saved <- readRDS(f)
  expect_null(attr(saved, "data", exact = TRUE))
  expect_identical(saved$population, p$population)
  before <- serialize(saved, NULL)

  for (style in c("methods", "concise", "extended")) {
    expect_no_warning(
      txt <- weasel_justify_subset(saved, "anchored_strict", style = style,
                                   cite = FALSE)
    )
    expect_identical(txt, weasel_justify_subset(p, "anchored_strict",
                                                style = style, cite = FALSE))
    expect_match(txt, "planning population comprised 6", fixed = TRUE)
    expect_match(txt, "out of 7 distinct respondent(s)", fixed = TRUE)
  }

  expect_identical(serialize(saved, NULL), before)
  expect_identical(weasel_apply(saved, "anchored_strict", data = d),
                    weasel_apply(p, "anchored_strict", data = d))
})

test_that("legacy plans omit population text rather than infer a denominator", {
  p <- weasel_plan(make_fixture(), "id", "time", lower = 4, upper = 6)
  legacy <- p
  legacy$population <- NULL
  legacy_light <- legacy
  attr(legacy_light, "data") <- NULL
  population_sentence <- paste0(
    "The planning population comprised 6 respondent(s) observed at least ",
    "once within the analysis window, out of 7 distinct respondent(s) in ",
    "the supplied data; retention figures are relative to this in-window population. "
  )

  for (old_plan in list(legacy, legacy_light)) {
    before <- serialize(old_plan, NULL)
    for (style in c("methods", "concise", "extended")) {
      expect_no_warning(
        txt <- weasel_justify_subset(old_plan, "anchored_strict", style = style,
                                     cite = FALSE)
      )
      complete <- weasel_justify_subset(p, "anchored_strict", style = style,
                                        cite = FALSE)
      expect_identical(txt, sub(population_sentence, "", complete, fixed = TRUE))
      expect_false(grepl("planning population|retention figures|distinct respondent", txt))
    }
    expect_identical(serialize(old_plan, NULL), before)
  }
})
