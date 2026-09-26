# stage 5 contracts: harmonized constraint vocabulary, deprecated
# aliases, and the min_present = 1 exploration default

test_that("deprecated scope aliases warn and reproduce the new names", {
  d <- make_fixture()

  ref <- suppressMessages({
    set_weasel_scope(d, "id", "time", min_present = 3, max_gap_len = 1,
                     n_gap_max = 1)
    pv <- suppressMessages(weasel_reshape_to_wide())
    weasel_clear_scope()
    pv$id
  })

  w <- capture_warnings(
    suppressMessages(
      set_weasel_scope(d, "id", "time", size = 3, gap = 1, n_gap = 1)
    )
  )
  expect_length(w, 3)
  expect_true(all(grepl("deprecated", w)))
  on.exit(weasel_clear_scope(), add = TRUE)
  pv_alias <- suppressMessages(weasel_reshape_to_wide())
  expect_setequal(pv_alias$id, ref)

  # the warning is classed for programmatic handling
  expect_warning(
    suppressMessages(set_weasel_scope(d, "id", "time", gap = 1)),
    class = "weasel_deprecated"
  )

  # size keeps its historical min() semantics through the alias
  suppressWarnings(set_weasel_scope(d, "id", "time", size = c(3, 8)))
  expect_equal(weasel:::the$scope$min_present, 3L)

  # an explicit new-name argument wins over its alias
  suppressWarnings(
    set_weasel_scope(d, "id", "time", min_present = 5, size = 2)
  )
  expect_equal(weasel:::the$scope$min_present, 5L)
})

test_that("size uses a representable minimum without converting larger values", {
  d <- make_fixture()
  on.exit(weasel_clear_scope(), add = TRUE)
  limit <- .Machine$integer.max
  for (value in list(limit, as.double(limit), c(limit + 1, limit))) {
    expect_no_warning(expect_warning(
      env <- set_weasel_scope(d, "id", "time", size = value),
      class = "weasel_deprecated"
    ))
    expect_identical(env$min_present, limit)
  }

  set_weasel_scope(d, "id", "time", min_present = 3)
  expected <- suppressMessages(weasel_reshape_to_wide())
  before <- serialize(d, NULL)
  for (value in list(c(8, 3, 3), c(3, limit + 1),
                     c(.Machine$double.xmax, 3),
                     c(3 + 1e-9, limit + 1, 3 - 1e-9))) {
    expect_no_warning(expect_warning(
      env <- set_weasel_scope(d, "id", "time", size = value),
      class = "weasel_deprecated"
    ))
    expect_identical(env$min_present, 3L)
    expect_identical(suppressMessages(weasel_reshape_to_wide()), expected)
  }
  expect_identical(serialize(d, NULL), before)
})

test_that("size rejects an unrepresentable minimum without changing scope", {
  d <- make_fixture()
  env <- set_weasel_scope(d, "id", "time", min_present = 3)
  on.exit(weasel_clear_scope(), add = TRUE)
  suppressMessages(weasel_reshape_to_wide())
  before <- serialize(as.list(env), NULL)
  for (value in list(2147483648, c(2147483649, 2147483648),
                     .Machine$double.xmax)) {
    expect_no_warning(expect_warning(expect_error(
      set_weasel_scope(d, "id", "time", size = value),
      "min_present.*between 0 and 2147483647", class = "weasel_error"
    ), class = "weasel_deprecated"))
    expect_identical(weasel:::the$scope, env)
    expect_identical(serialize(as.list(env), NULL), before)
  }
})

test_that("size still validates every element before taking its minimum", {
  d <- make_fixture()
  on.exit(weasel_clear_scope(), add = TRUE)
  for (value in list(numeric(), TRUE, "3", factor("3"), list(3), 3 + 0i,
                     c(3, NA), c(3, NaN), c(3, Inf), c(3, -Inf),
                     c(3, 0), c(3, -1), c(3, 1 - 1e-9),
                     c(3, 4.5), c(2147483648, 3.5))) {
    expect_no_warning(expect_warning(expect_error(
      set_weasel_scope(d, "id", "time", size = value),
      "size must be a vector of positive integers", class = "weasel_error"
    ), class = "weasel_deprecated"))
  }
  expect_no_warning(env <- set_weasel_scope(d, "id", "time", size = NULL))
  expect_identical(env$min_present, 1L)
})

test_that("explicit min_present still takes precedence over size", {
  d <- make_fixture()
  on.exit(weasel_clear_scope(), add = TRUE)
  for (value in list(2147483648, c(NA, 2147483648), "invalid")) {
    expect_no_warning(expect_warning(
      env <- set_weasel_scope(d, "id", "time", min_present = 3, size = value),
      class = "weasel_deprecated"
    ))
    expect_identical(env$min_present, 3L)
    expect_no_warning(expect_warning(expect_error(
      set_weasel_scope(d, "id", "time", min_present = NULL, size = value),
      "min_present must be a single integer >= 1", class = "weasel_error"
    ), class = "weasel_deprecated"))
    expect_identical(weasel:::the$scope, env)
  }
})

test_that("size minimum validation retains argument and deprecation order", {
  d <- make_fixture()
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_no_warning(expect_error(
    set_weasel_scope(d, "id", "time", lower = Inf, size = 2147483648),
    "lower", class = "weasel_error"
  ))
  expect_no_warning(expect_warning(expect_error(
    set_weasel_scope(d, "id", "time", size = c(2147483648, 3.5), gap = 1),
    "size must be a vector of positive integers", class = "weasel_error"
  ), class = "weasel_deprecated"))

  warnings <- list()
  expect_error(withCallingHandlers(
    set_weasel_scope(d, "id", "time", size = 2147483648,
                     gap = -1, n_gap = -1, max_missing = -1),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  ), "min_present.*between 0 and 2147483647", class = "weasel_error")
  expect_length(warnings, 3L)
  expect_true(all(vapply(warnings, inherits, logical(1), "weasel_deprecated")))
  expect_identical(vapply(warnings, conditionMessage, character(1)), c(
    "argument 'size' is deprecated; use 'min_present' instead.",
    "argument 'gap' is deprecated; use 'max_gap_len' instead.",
    "argument 'n_gap' is deprecated; use 'n_gap_max' instead."
  ))
})

test_that("min_present defaults to 1: exploration shows everyone", {
  d <- rbind(make_fixture(),
             data.frame(id = "solo", time = 5, var1 = 0))
  suppressMessages(set_weasel_scope(d, "id", "time"))
  on.exit(weasel_clear_scope(), add = TRUE)
  pv <- suppressMessages(weasel_reshape_to_wide())
  # the single-wave respondent survives (dropped under the old default 3)
  expect_true("solo" %in% pv$id)
  expect_equal(nrow(pv), 8L)
})

test_that("scope max_missing and require_endpoints constrain retention", {
  d <- make_fixture()
  suppressMessages(
    set_weasel_scope(d, "id", "time", max_missing = 1,
                     require_endpoints = TRUE)
  )
  on.exit(weasel_clear_scope(), add = TRUE)
  pv <- suppressMessages(weasel_reshape_to_wide())
  # complete cases plus b1 (one missing wave, both endpoints)
  expect_setequal(pv$id, c("a1", "a2", "b1"))
})

test_that("legacy scenario column max_gap_max still works, with a warning", {
  d <- make_fixture()
  legacy <- data.frame(
    scenario = "one_gap", require_endpoints = FALSE,
    max_missing = 8, n_gap_max = 8, max_gap_max = 1
  )
  expect_warning(
    p_legacy <- weasel_plan(d, "id", "time", span = "full",
                            scenarios = legacy),
    class = "weasel_deprecated"
  )
  modern <- data.frame(
    scenario = "one_gap", require_endpoints = FALSE,
    max_missing = 8, n_gap_max = 8, max_gap_len = 1
  )
  p_modern <- weasel_plan(d, "id", "time", span = "full",
                          scenarios = modern)
  expect_setequal(p_legacy$plan$ids[[1]], p_modern$plan$ids[[1]])
  expect_true("max_gap_len" %in% names(p_legacy$plan))
})

test_that("weasel_sensitivity's max_gap_max alias warns and maps", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  expect_warning(
    s_alias <- weasel_sensitivity(p, max_missing = 0:1, n_gap_max = 0:1,
                                  max_gap_max = 0:1),
    class = "weasel_deprecated"
  )
  s_new <- weasel_sensitivity(p, max_missing = 0:1, n_gap_max = 0:1,
                              max_gap_len = 0:1)
  expect_equal(s_alias, s_new)
  expect_true("max_gap_len" %in% names(s_new))
  expect_false("max_gap_max" %in% names(s_new))
})

test_that("justification reads gap limits from old and new plan columns", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  txt_new <- weasel_justify_subset(p, "anchored_balanced")
  expect_match(txt_new, "no longer than 1 wave", fixed = TRUE)

  # simulate a pre-0.4 plan whose table still has max_gap_max
  legacy <- p
  names(legacy$plan)[names(legacy$plan) == "max_gap_len"] <- "max_gap_max"
  txt_old <- weasel_justify_subset(legacy, "anchored_balanced")
  expect_match(txt_old, "no longer than 1 wave", fixed = TRUE)
})
