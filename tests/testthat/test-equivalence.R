# cross-pipeline equivalence: identical data, span, grid, and structural
# constraints must retain identical respondents through the scope and
# plan pipelines; these tests lock the shared vocabulary in place

equiv_scenario <- function(min_present, gap, n_gap, L) {
  data.frame(
    scenario          = "equiv",
    require_endpoints = FALSE,
    max_missing       = L - min_present,
    n_gap_max         = if (is.null(n_gap)) L else n_gap,
    max_gap_max       = if (is.null(gap)) L else gap,
    stringsAsFactors  = FALSE
  )
}

scope_kept_ids <- function(d, size, gap, n_gap, grid = "consecutive") {
  suppressMessages(
    set_weasel_scope(d, "id", "time", size = size, gap = gap,
                     n_gap = n_gap, grid = grid)
  )
  on.exit(weasel_clear_scope(), add = TRUE)
  pv <- suppressMessages(suppressWarnings(weasel_reshape_to_wide()))
  pv$id
}

test_that("scope constraints equal an equivalent plan scenario (consecutive)", {
  d <- generate_weasel_dummy_data(n_ids = 80, n_times = 8, seed = 71)
  p0 <- weasel_plan(d, "id", "time", span = "full")
  L <- length(p0$span)

  combos <- list(
    list(size = 1L, gap = NULL, n_gap = NULL),
    list(size = 3L, gap = 1L,   n_gap = NULL),
    list(size = 4L, gap = 2L,   n_gap = 1L),
    list(size = 6L, gap = 1L,   n_gap = 1L)
  )
  for (cb in combos) {
    kept_scope <- scope_kept_ids(d, cb$size, cb$gap, cb$n_gap)
    p <- weasel_plan(d, "id", "time", span = "full",
                     scenarios = equiv_scenario(cb$size, cb$gap, cb$n_gap, L))
    expect_setequal(kept_scope, p$plan$ids[[1]])
  }
})

test_that("scope constraints equal an equivalent plan scenario (observed)", {
  d <- generate_weasel_dummy_data(n_ids = 50, waves = seq(2008, 2020, 2),
                                  seed = 72)
  p0 <- weasel_plan(d, "id", "time", span = "full", grid = "observed")
  L <- length(p0$span)

  kept_scope <- scope_kept_ids(d, size = 4L, gap = 1L, n_gap = 1L,
                               grid = "observed")
  p <- weasel_plan(d, "id", "time", span = "full", grid = "observed",
                   scenarios = equiv_scenario(4L, 1L, 1L, L))
  expect_setequal(kept_scope, p$plan$ids[[1]])
})

test_that("scope and plan compute identical per-respondent metrics", {
  d <- generate_weasel_dummy_data(n_ids = 60, n_times = 9, seed = 73)
  suppressMessages(set_weasel_scope(d, "id", "time", size = 1))
  on.exit(weasel_clear_scope(), add = TRUE)
  suppressMessages(weasel_reshape_to_wide())
  sm <- weasel:::the$scope$scope_metrics

  p <- weasel_plan(d, "id", "time", span = "full")
  pm <- p$id_metrics

  sm <- sm[order(sm$id), ]
  pm <- pm[order(pm$id), ]
  expect_equal(sm$id, pm$id)
  expect_equal(sm$n_present, pm$n_present)
  expect_equal(sm$n_gap, pm$n_gap)
  expect_equal(sm$max_gap, pm$max_gap)
  expect_equal(sm$has_lower, pm$has_lower)
  expect_equal(sm$has_upper, pm$has_upper)
})

test_that("duplicate rows change what neither pipeline retains", {
  d <- generate_weasel_dummy_data(n_ids = 40, n_times = 7, seed = 74)
  dd <- rbind(d, d[seq(1, nrow(d), by = 9), ])

  kept_clean <- scope_kept_ids(d, 3L, 1L, NULL)
  kept_dup   <- scope_kept_ids(dd, 3L, 1L, NULL)
  expect_setequal(kept_clean, kept_dup)

  p_clean <- weasel_plan(d, "id", "time", span = "full")
  p_dup   <- suppressWarnings(weasel_plan(dd, "id", "time", span = "full"))
  expect_equal(p_clean$id_metrics, p_dup$id_metrics)
})
