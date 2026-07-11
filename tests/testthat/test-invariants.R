# algebraic and structural invariants that must hold for any panel;
# random panels use fixed seeds, so any failure reproduces exactly

random_panel <- function(seed, n_ids = 60, n_times = 9) {
  generate_weasel_dummy_data(n_ids = n_ids, n_times = n_times, seed = seed)
}

test_that("id_metrics satisfies its algebraic identities on random panels", {
  for (seed in c(11, 12, 13)) {
    d <- random_panel(seed)
    p <- weasel_plan(d, "id", "time", span = "full")
    m <- p$id_metrics
    L <- length(p$span)

    expect_equal(m$n_present + m$n_missing, rep(L, nrow(m)))
    expect_equal(m$prop_present, m$n_present / L)
    expect_true(all(m$n_present >= 1))
    # a respondent has interior gaps if and only if the longest one is > 0
    expect_true(all((m$n_gap == 0) == (m$max_gap == 0)))
    # no interior gap can exceed the number of missing waves
    expect_true(all(m$max_gap <= m$n_missing))
    # both endpoints observed implies at least two observed waves
    both_ends <- m$has_lower & m$has_upper
    if (L > 1) expect_true(all(m$n_present[both_ends] >= 2))
  }
})

test_that("scenario id lists agree exactly with the stored metrics", {
  d <- random_panel(21)
  p <- weasel_plan(d, "id", "time", span = "core")
  m <- p$id_metrics

  for (i in seq_len(nrow(p$plan))) {
    row <- p$plan[i, , drop = FALSE]
    ids <- row$ids[[1]]
    expect_true(all(ids %in% m$id))
    expect_equal(row$n_ids, length(ids))

    qual <- m$n_missing <= row$max_missing &
      m$n_gap <= row$n_gap_max &
      m$max_gap <= row$max_gap_max
    if (isTRUE(row$require_endpoints)) {
      qual <- qual & m$has_lower & m$has_upper
    }
    # every retained id qualifies, and every qualifying id is retained
    expect_setequal(m$id[qual], ids)
  }
})

test_that("weasel_apply returns exactly the scenario ids, within the span", {
  d <- random_panel(31)
  p <- weasel_plan(d, "id", "time", span = "core")
  for (sc in p$plan$scenario) {
    ids <- p$plan$ids[p$plan$scenario == sc][[1]]
    if (length(ids) == 0) next
    sub <- weasel_apply(p, sc)
    expect_setequal(unique(sub$id), ids)
    expect_true(all(as.integer(round(sub$time)) %in% p$span))
  }
})

test_that("plan results are invariant to input row order", {
  d <- random_panel(41)
  set.seed(1)
  d2 <- d[sample(nrow(d)), ]
  p1 <- weasel_plan(d, "id", "time", span = "core")
  p2 <- weasel_plan(d2, "id", "time", span = "core")

  drop_ids <- function(x) x[setdiff(names(x), "ids")]
  expect_equal(drop_ids(p1$plan), drop_ids(p2$plan))
  expect_equal(p1$id_metrics, p2$id_metrics)
  expect_true(all(mapply(setequal, p1$plan$ids, p2$plan$ids)))
})

test_that("pattern counts and pattern strings add up", {
  d <- random_panel(51)
  set_weasel_scope(d, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  pv <- suppressMessages(weasel_reshape_to_wide())
  v <- weasel_summarize_waves()

  # every retained respondent is counted in exactly one pattern
  expect_equal(sum(v$ids), nrow(pv))
  # the pattern string encodes n: dots (missing) plus n equal span length
  dots <- nchar(gsub("[^.]", "", v$waves))
  expect_equal(v$n + dots, rep(ncol(pv) - 1L, nrow(v)))
})

test_that("weasel_sensitivity matches brute-force filtering of id_metrics", {
  d <- random_panel(61)
  p <- weasel_plan(d, "id", "time", span = "core")
  m <- p$id_metrics
  s <- weasel_sensitivity(p, require_endpoints = c(TRUE, FALSE),
                          max_missing = 0:2, n_gap_max = 0:1,
                          max_gap_max = 0:2)
  for (i in seq_len(nrow(s))) {
    keep <- m$n_missing <= s$max_missing[i] &
      m$n_gap <= s$n_gap_max[i] &
      m$max_gap <= s$max_gap_max[i]
    if (s$require_endpoints[i]) keep <- keep & m$has_lower & m$has_upper
    expect_identical(s$n_ids[i], as.integer(sum(keep)))
    if (any(keep)) {
      expect_equal(s$mean_prop_present[i], mean(m$prop_present[keep]))
    } else {
      expect_true(is.na(s$mean_prop_present[i]))
    }
  }
})
