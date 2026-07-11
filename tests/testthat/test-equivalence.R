# cross-pipeline equivalence: identical data, span, grid, and structural
# constraints must retain identical respondents through the scope and
# plan pipelines; these tests lock the shared vocabulary in place

equiv_scenario <- function(min_present, max_gap_len, n_gap_max, L,
                           require_endpoints = FALSE) {
  data.frame(
    scenario          = "equiv",
    require_endpoints = require_endpoints,
    max_missing       = L - min_present,
    n_gap_max         = if (is.null(n_gap_max)) L else n_gap_max,
    max_gap_len       = if (is.null(max_gap_len)) L else max_gap_len,
    stringsAsFactors  = FALSE
  )
}

scope_kept_ids <- function(d, min_present, max_gap_len, n_gap_max,
                           grid = "consecutive",
                           require_endpoints = FALSE) {
  suppressMessages(
    set_weasel_scope(d, "id", "time", min_present = min_present,
                     max_gap_len = max_gap_len, n_gap_max = n_gap_max,
                     require_endpoints = require_endpoints, grid = grid)
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
    list(mp = 1L, gl = NULL, ng = NULL),
    list(mp = 3L, gl = 1L,   ng = NULL),
    list(mp = 4L, gl = 2L,   ng = 1L),
    list(mp = 6L, gl = 1L,   ng = 1L)
  )
  for (cb in combos) {
    kept_scope <- scope_kept_ids(d, cb$mp, cb$gl, cb$ng)
    p <- weasel_plan(d, "id", "time", span = "full",
                     scenarios = equiv_scenario(cb$mp, cb$gl, cb$ng, L))
    expect_setequal(kept_scope, p$plan$ids[[1]])
  }

  # endpoints: the scope constraint equals an anchored scenario
  kept_anchored <- scope_kept_ids(d, 3L, 1L, NULL,
                                  require_endpoints = TRUE)
  p_anch <- weasel_plan(d, "id", "time", span = "full",
                        scenarios = equiv_scenario(3L, 1L, NULL, L,
                                                   require_endpoints = TRUE))
  expect_setequal(kept_anchored, p_anch$plan$ids[[1]])
})

test_that("scope constraints equal an equivalent plan scenario (observed)", {
  d <- generate_weasel_dummy_data(n_ids = 50, waves = seq(2008, 2020, 2),
                                  seed = 72)
  p0 <- weasel_plan(d, "id", "time", span = "full", grid = "observed")
  L <- length(p0$span)

  kept_scope <- scope_kept_ids(d, min_present = 4L, max_gap_len = 1L,
                               n_gap_max = 1L, grid = "observed")
  p <- weasel_plan(d, "id", "time", span = "full", grid = "observed",
                   scenarios = equiv_scenario(4L, 1L, 1L, L))
  expect_setequal(kept_scope, p$plan$ids[[1]])
})

test_that("scope and plan compute identical per-respondent metrics", {
  d <- generate_weasel_dummy_data(n_ids = 60, n_times = 9, seed = 73)
  suppressMessages(set_weasel_scope(d, "id", "time", min_present = 1))
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
