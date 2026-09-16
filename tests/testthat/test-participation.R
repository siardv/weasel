# participation depends on valid respondent-wave rows, not item values
participation_fixture <- function(grid = "consecutive", id_transform = identity) {
  d <- data.frame(
    id = c(rep("A", 4), rep("B", 2), rep("C", 2), "D", "E",
           NA, "A", NA, "A", "A", "E", "E"),
    time = c(1, 2, 4, 5, 1, 5, 2, 4, 1, 0, 3, NA, NA, 1, 2, 0, 0),
    value = c(rep(NA_real_, 9), 10:17),
    stringsAsFactors = FALSE
  )
  if (grid == "observed") {
    d$time <- c(1990, 2000, 2002, 2005, 2009, 2014)[d$time + 1L]
  }
  # the near-integer duplicate still represents the same participation wave
  d$time[15] <- d$time[15] + 1e-9
  d$id <- id_transform(d$id)
  d
}

participation_scenario <- function() {
  data.frame(scenario = "anchored", require_endpoints = TRUE,
             max_missing = 1, n_gap_max = 1, max_gap_len = 1)
}

test_that("dirty rows preserve hand-computed participation across both pipelines", {
  on.exit(weasel_clear_scope(), add = TRUE)
  representations <- list(
    character = identity,
    factor = function(x) factor(x, levels = c("unused", "D", "C", "B", "A", "E")),
    integer = function(x) match(x, LETTERS[1:5])
  )
  for (grid in c("consecutive", "observed")) {
    span <- if (grid == "consecutive") 1:5 else c(2000L, 2002L, 2005L, 2009L, 2014L)
    for (transform_id in representations) {
      d <- participation_fixture(grid, transform_id)
      expected <- data.frame(
        id = transform_id(c("A", "B", "C", "D")),
        n_present = c(4L, 2L, 2L, 1L), n_missing = c(1L, 3L, 3L, 4L),
        prop_present = c(0.8, 0.4, 0.4, 0.2),
        has_lower = c(TRUE, TRUE, FALSE, TRUE),
        has_upper = c(TRUE, TRUE, FALSE, FALSE),
        n_gap = c(1L, 1L, 1L, 0L), max_gap = c(1L, 3L, 1L, 0L)
      )
      expected <- expected[order(expected$id), ]
      rownames(expected) <- NULL
      p <- suppressMessages(suppressWarnings(
        weasel_plan(d, "id", "time", lower = span[1], upper = tail(span, 1),
                    grid = grid, scenarios = participation_scenario())
      ))
      env <- suppressMessages(
        set_weasel_scope(d, "id", "time", lower = span[1], upper = tail(span, 1),
                         grid = grid, min_present = 4, max_gap_len = 1,
                         n_gap_max = 1, require_endpoints = TRUE)
      )
      pivot <- suppressMessages(suppressWarnings(weasel_reshape_to_wide()))
      expect_identical(p$span, span)
      expect_identical(env$span, span)
      expect_equal(p$id_metrics, expected)
      expect_equal(env$scope_metrics, expected[names(env$scope_metrics)])
      expect_identical(p$plan$ids[[1]], transform_id("A"))
      expect_identical(pivot$id, p$plan$ids[[1]])
      expect_identical(as.integer(pivot[1, -1]), replace(span, 3, NA_integer_))

      # reversing rows and filling item values cannot change participation
      d_reversed <- d[rev(seq_len(nrow(d))), ]
      d_reversed$value <- 1
      p_reversed <- suppressMessages(suppressWarnings(
        weasel_plan(d_reversed, "id", "time", lower = span[1],
                    upper = tail(span, 1), grid = grid,
                    scenarios = participation_scenario())
      ))
      expect_equal(p_reversed$id_metrics, expected)
      expect_identical(p_reversed$plan$ids, p$plan$ids)
    }
  }
})

test_that("participation is deduplicated while retained long rows stay intact", {
  d <- participation_fixture()
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_warning(
    p <- suppressMessages(weasel_plan(d, "id", "time", lower = 1, upper = 5,
                                      scenarios = participation_scenario())),
    class = "weasel_duplicates"
  )
  suppressMessages(set_weasel_scope(d, "id", "time", lower = 1, upper = 5))
  expect_warning(suppressMessages(weasel_reshape_to_wide()),
                 "2 duplicated", class = "weasel_duplicates")
  expect_warning(out <- weasel_apply(p, "anchored"),
                 "2 duplicated", class = "weasel_duplicates")
  expect_identical(out, d[c(1:4, 14:15), ])
  expect_identical(p$id_metrics$n_present[p$id_metrics$id == "A"], 4L)
})

test_that("outside-span duplicates retain the callers' warning boundaries", {
  d <- data.frame(id = c(rep("A", 3), "B", "B"), time = c(1:3, 0, 0))
  on.exit(weasel_clear_scope(), add = TRUE)
  expect_warning(
    p <- suppressMessages(weasel_plan(d, "id", "time", lower = 1, upper = 3)),
    "1 duplicated", class = "weasel_duplicates"
  )
  suppressMessages(set_weasel_scope(d, "id", "time", lower = 1, upper = 3))
  expect_no_warning(pivot <- suppressMessages(weasel_reshape_to_wide()))
  expect_identical(pivot$id, "A")
  expect_identical(p$id_metrics$id, "A")
})

test_that("a resolved span with no valid participants preserves caller errors", {
  d <- data.frame(id = factor(c("A", NA, "B")), time = 1:3)
  on.exit(weasel_clear_scope(), add = TRUE)
  for (grid in c("consecutive", "observed")) {
    expect_error(
      suppressMessages(weasel_plan(d, "id", "time", lower = 2, upper = 2,
                                   grid = grid)),
      "no usable ids found in the chosen span", fixed = TRUE
    )
    suppressMessages(set_weasel_scope(d, "id", "time", lower = 2, upper = 2,
                                     grid = grid))
    expect_error(suppressMessages(weasel_reshape_to_wide()),
                 "no rows in the selected span", fixed = TRUE)
  }
})
