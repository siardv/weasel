test_that("all styles describe unlimited and mixed tolerance dimensions", {
  scenarios <- expand.grid(max_missing = c(0, 1, 2, Inf),
                           n_gap_max = c(0, 2, Inf),
                           max_gap_len = c(0, 2, Inf))
  scenarios$scenario <- paste0("case_", seq_len(nrow(scenarios)))
  scenarios$require_endpoints <- TRUE
  p <- weasel_plan(make_fixture(), "id", "time", span = "full",
                    scenarios = scenarios)

  for (i in seq_len(nrow(scenarios))) {
    limits <- scenarios[i, ]
    for (style in c("methods", "concise", "extended")) {
      expect_no_warning(
        txt <- weasel_justify_subset(p, limits$scenario, style = style,
                                     cite = FALSE)
      )
      expect_false(grepl("\\b(Inf|NA|NaN)\\b", txt))
      expect_match(txt, "required observed endpoints", fixed = TRUE)

      if (is.infinite(limits$max_missing)) {
        expect_match(txt, "no separate limit on the number of missing waves",
                     fixed = TRUE)
      } else if (limits$max_missing == 0) {
        expect_match(txt, "required complete participation", fixed = TRUE)
      } else if (limits$max_missing == 1) {
        expect_match(txt, "allowed at most one missing wave", fixed = TRUE)
      } else {
        expect_match(txt, "allowed up to 2 missing waves", fixed = TRUE)
      }

      if (is.infinite(limits$n_gap_max) && is.infinite(limits$max_gap_len)) {
        expect_match(txt, "no separate limits on the number or length of interior missing blocks",
                     fixed = TRUE)
      } else {
        if (is.infinite(limits$n_gap_max)) {
          expect_match(txt, "no separate limit on the number of interior missing blocks",
                       fixed = TRUE)
        } else {
          expect_match(txt, sprintf("at most %s interior missing block(s)",
                                    limits$n_gap_max), fixed = TRUE)
        }
        if (is.infinite(limits$max_gap_len)) {
          expect_match(txt, "no separate limit on block length", fixed = TRUE)
        } else {
          expect_match(txt, sprintf("each no longer than %s wave(s)",
                                    limits$max_gap_len), fixed = TRUE)
        }
      }
    }
  }
})

test_that("unlimited text works with imported limits and saved legacy plans", {
  d <- make_fixture()
  scenarios <- data.frame(
    scenario = c("all_unlimited", "count_only", "length_only"),
    require_endpoints = FALSE,
    max_missing = factor(c("Inf", "Inf", "2")),
    n_gap_max = c("Inf", "1", "Inf"),
    max_gap_len = factor(c("Inf", "Inf", "1"))
  )
  p <- weasel_plan(d, "id", "time", span = "full", scenarios = scenarios,
                    keep_data = FALSE)
  legacy <- p
  names(legacy$plan)[names(legacy$plan) == "max_gap_len"] <- "max_gap_max"
  legacy$population <- NULL
  legacy$span <- NULL
  legacy$grid <- NULL
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(legacy, f)
  legacy <- readRDS(f)
  reference <- p
  reference$population <- NULL
  before <- serialize(legacy, NULL)

  for (scenario in scenarios$scenario) {
    for (style in c("methods", "concise", "extended")) {
      expect_no_warning(
        txt <- weasel_justify_subset(legacy, scenario, style = style, cite = FALSE)
      )
      expect_identical(txt, weasel_justify_subset(reference, scenario,
                                                  style = style, cite = FALSE))
      expect_match(txt, "did not require observed endpoints", fixed = TRUE)
    }
    expect_identical(weasel_apply(legacy, scenario, data = d),
                      weasel_apply(p, scenario, data = d))
  }
  expect_identical(serialize(legacy, NULL), before)
  expect_identical(weasel_apply(legacy, "all_unlimited", data = d), d)
})

test_that("unknown tolerance metadata retains its existing fallback", {
  p <- weasel_plan(make_fixture(), "id", "time", span = "full")
  for (missing in list(NULL, NA_real_)) {
    legacy <- p
    legacy$plan$max_missing <- missing
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(legacy, "lenient", style = style, cite = FALSE)
      expect_match(txt, "applied an explicit limit on missing waves", fixed = TRUE)
      expect_false(grepl("no separate limit", txt, fixed = TRUE))
    }

    for (gap_col in c("n_gap_max", "max_gap_len")) {
      legacy <- p
      legacy$plan[[gap_col]] <- missing
      for (style in c("methods", "concise", "extended")) {
        txt <- weasel_justify_subset(legacy, "lenient", style = style, cite = FALSE)
        expect_false(grepl("and restricted the missingness structure|no separate limit", txt))
      }
    }
  }
})

test_that("numeric tolerance rendering keeps count semantics without overflow", {
  d <- make_fixture()
  scenarios <- data.frame(
    scenario = c("near_integer", "million", "large_finite"),
    require_endpoints = FALSE,
    max_missing = c(1 - 1e-9, 1e6, 2147483648),
    n_gap_max = c(1 + 1e-9, 1e6, 2147483648),
    max_gap_len = c(2 + 1e-9, 1e6, 2147483648)
  )
  p <- weasel_plan(d, "id", "time", span = "full", scenarios = scenarios)
  for (style in c("methods", "concise", "extended")) {
    near <- weasel_justify_subset(p, "near_integer", style = style, cite = FALSE)
    expect_match(near, "required complete participation", fixed = TRUE)
    expect_match(near, "at most 1 interior missing block(s), each no longer than 2 wave(s)",
                 fixed = TRUE)
    million <- weasel_justify_subset(p, "million", style = style, cite = FALSE)
    expect_match(million, "allowed up to 1000000 missing waves", fixed = TRUE)
    expect_match(million, "at most 1000000 interior missing block(s), each no longer than 1000000 wave(s)",
                 fixed = TRUE)
    expect_no_warning(
      large <- weasel_justify_subset(p, "large_finite", style = style, cite = FALSE)
    )
    expect_match(large, "allowed up to 2147483648 missing waves", fixed = TRUE)
    expect_match(large, "at most 2147483648 interior missing block(s), each no longer than 2147483648 wave(s)",
                 fixed = TRUE)
    expect_false(grepl("no separate limit|\\b(NA|Inf)\\b", large))
  }
})
