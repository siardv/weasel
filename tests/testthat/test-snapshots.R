# snapshot tests for the user-visible textual surfaces, frozen after
# the 0.4.0 interface work; when one of these changes, the diff is the
# review artifact

snap_plan <- function() {
  weasel_plan(make_fixture(), "id", "time", span = "full")
}

test_that("plan print output is stable", {
  p <- snap_plan()
  expect_snapshot(print(p))
})

test_that("scope info output is stable", {
  d <- make_fixture()
  suppressMessages(set_weasel_scope(d, "id", "time", min_present = 2,
                                    max_gap_len = 1))
  on.exit(weasel_clear_scope(), add = TRUE)
  suppressMessages(weasel_reshape_to_wide())
  suppressMessages(weasel_summarize_waves())
  expect_snapshot(weasel_scope_info())
})

test_that("comparison and subset sentences are stable", {
  p <- snap_plan()
  cmp <- weasel_compare_scenarios(p)
  expect_snapshot(cat(weasel_compare_to_sentence(cmp)))
  s <- weasel_summarize_subset(p, "anchored_balanced")
  expect_snapshot(cat(weasel_subset_to_sentence(s)))
})

test_that("justification paragraphs are stable", {
  p <- snap_plan()
  expect_snapshot(cat(weasel_justify_subset(p, "anchored_balanced")))
  expect_snapshot(cat(weasel_justify_subset(p, "anchored_strict",
                                            style = "concise")))
  expect_snapshot(cat(weasel_justify_subset(p, "lenient",
                                            style = "extended")))
})
