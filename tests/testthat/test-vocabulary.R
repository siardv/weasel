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
