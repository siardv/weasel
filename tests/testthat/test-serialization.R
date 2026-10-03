# plans are saveable objects; reunion with the workflow must be lossless

test_that("plans survive saveRDS/readRDS and keep working", {
  d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 81)
  p <- weasel_plan(d, "id", "time", span = "core")
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(p, f)
  q <- readRDS(f)

  expect_identical(class(q), "weasel_plan")
  expect_equal(weasel_apply(p, "lenient"), weasel_apply(q, "lenient"))
  expect_equal(weasel_sensitivity(p, max_missing = 0:1),
               weasel_sensitivity(q, max_missing = 0:1))
  expect_equal(weasel_summarize_subset(p, "lenient")$headline,
               weasel_summarize_subset(q, "lenient")$headline)
})

test_that("keep_data = FALSE plans serialize small and reunite with data", {
  d <- generate_weasel_dummy_data(n_ids = 200, n_times = 10, seed = 82)
  p_full  <- weasel_plan(d, "id", "time", span = "full")
  p_light <- weasel_plan(d, "id", "time", span = "full", keep_data = FALSE)

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  saveRDS(p_full, f1)
  saveRDS(p_light, f2)
  expect_lt(file.size(f2), file.size(f1))

  q <- readRDS(f2)
  expect_equal(weasel_apply(q, "lenient", data = d),
               weasel_apply(p_full, "lenient"))
})

test_that("legacy plans without $span apply via the documented fallback", {
  d <- generate_weasel_dummy_data(n_ids = 40, n_times = 7, seed = 83)
  p <- weasel_plan(d, "id", "time", span = "full")
  legacy <- p
  legacy$span <- NULL  # pre-0.3 objects stored only the bounds
  # regression: $span used to partial-match span_reason ("full"), coerce
  # to NA, and silently return zero rows; [["span"]] indexing fixed it
  sub_legacy <- weasel_apply(legacy, "lenient")
  expect_gt(nrow(sub_legacy), 0)
  # on a consecutive grid the lower:upper fallback rebuilds the same span
  expect_equal(sub_legacy, weasel_apply(p, "lenient"))
  expect_equal(weasel_summarize_subset(legacy, "lenient")$headline,
               weasel_summarize_subset(p, "lenient")$headline)
  out <- capture.output(print(legacy))
  expect_true(any(grepl("span: 1:7", out, fixed = TRUE)))
})

serialization_q6_capture <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(expr, warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

test_that("restored v2 plans warn once and retain their saved selections", {
  cases <- lapply(list(c(1, 1 + .Machine$double.eps), c(1e15, 1e15 + 1)),
                  function(ids) {
    original <- data.frame(id = ids[c(1, 1, 1, 2, 2)],
                           time = c(1, 2, 3, 1, 2), x = 1:5)
    changed <- original
    changed$id[3] <- ids[2]
    list(original = original, changed = changed, max_missing = 0)
  })
  # the fourth row leaves a retained and an excluded group for selectivity
  cases[[3]] <- list(
    original = data.frame(id = c("A\x1f2\x1eB", "C", "D", "D"),
                          time = c(2L, 3L, 4L, 2L), x = 1:4),
    changed = data.frame(id = c("A", "B\x1f2\x1eC", "D", "D"),
                         time = c(2L, 3L, 4L, 2L), x = 1:4),
    max_missing = 1
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  for (case in cases) {
    scenario <- data.frame(scenario = "strict", require_endpoints = TRUE,
                           max_missing = case$max_missing,
                           n_gap_max = 1, max_gap_len = 1)
    fresh <- suppressMessages(weasel_plan(case$changed, "id", "time",
                                          span = "full", scenarios = scenario))
    for (keep_data in c(TRUE, FALSE)) {
      p <- suppressMessages(weasel_plan(case$original, "id", "time",
                                       span = "full", scenarios = scenario,
                                       keep_data = keep_data))
      saveRDS(p, path)
      for (q in list(readRDS(path), unserialize(serialize(p, NULL)))) {
        expect_identical(q, p)
        before <- serialize(q, NULL)
        expected <- case$changed[
          case$changed$id %in% q$plan$ids[[1]], , drop = FALSE
        ]
        for (reunite in list(weasel_apply, weasel_summarize_subset,
                            weasel_selectivity)) {
          same <- serialization_q6_capture(
            reunite(q, "strict", data = case$original)
          )
          expect_length(same$warnings, 0L)
          changed <- serialization_q6_capture(
            reunite(q, "strict", data = case$changed)
          )
          expect_length(changed$warnings, 1L)
          expect_s3_class(changed$warnings[[1]], "weasel_data_mismatch")
          expect_match(conditionMessage(changed$warnings[[1]]),
                       "pair digest mismatch", fixed = TRUE)
          expect_identical(serialize(q, NULL), before)
          if (identical(reunite, weasel_apply)) {
            expect_identical(changed$value, expected)
          } else if (identical(reunite, weasel_summarize_subset)) {
            expect_identical(changed$value$data, expected)
          } else {
            unguarded <- q
            unguarded$fingerprint <- NULL
            expect_identical(changed$value,
                             reunite(unguarded, "strict", data = case$changed))
          }
        }
        if (is.numeric(case$original$id)) {
          expect_false(identical(q$plan$ids, fresh$plan$ids))
          expect_identical(nrow(expected), 2L)
          expect_identical(nrow(weasel_apply(fresh, "strict")), 3L)
        }
      }
    }
  }
})

test_that("restored fingerprints route by their stored version only", {
  d <- data.frame(id = c(1, 1, 1, 1 + .Machine$double.eps,
                        1 + .Machine$double.eps),
                  time = c(1, 2, 3, 1, 2), x = 1:5)
  changed <- d
  changed$id[3] <- 1 + .Machine$double.eps
  scenario <- data.frame(scenario = "strict", require_endpoints = TRUE,
                         max_missing = 0, n_gap_max = 0, max_gap_len = 0)
  p <- suppressMessages(weasel_plan(d, "id", "time", span = "full",
                                   scenarios = scenario, keep_data = FALSE))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  for (version in list(NULL, 1L, 1, 2L, 2)) {
    legacy <- p
    route <- if (is.null(version)) 1L else version
    legacy$fingerprint <- weasel:::.weasel_data_fingerprint(d, "id", "time", route)
    if (!is.null(version)) legacy$fingerprint$encoding_version <- version
    saveRDS(legacy, path)
    for (q in list(readRDS(path), unserialize(serialize(legacy, NULL)))) {
      before <- serialize(q, NULL)
      for (reunite in list(weasel_apply, weasel_summarize_subset,
                          weasel_selectivity)) {
        expect_no_warning(reunite(q, "strict", data = d))
        out <- serialization_q6_capture(reunite(q, "strict", data = changed))
        expect_length(out$warnings, if (route == 2L) 1L else 0L)
        expect_identical(serialize(q, NULL), before)
      }
    }
  }
})

test_that("unsupported fingerprint versions fail explicitly after restoration", {
  d <- data.frame(id = c("A", "A", "A", "B", "B"),
                  time = c(1, 2, 3, 1, 2), x = 1:5)
  scenario <- data.frame(scenario = "strict", require_endpoints = TRUE,
                         max_missing = 0, n_gap_max = 0, max_gap_len = 0)
  p <- suppressMessages(weasel_plan(d, "id", "time", span = "full",
                                   scenarios = scenario))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  for (version in list(NULL, 0L, 3L, "2", TRUE, NA_integer_, NaN, Inf,
                       c(1L, 2L), factor("2"), c(version = 2L))) {
    unsupported <- p
    # assigning NULL with $ would delete the field instead of storing it
    unsupported$fingerprint["encoding_version"] <- list(version)
    saveRDS(unsupported, path)
    for (q in list(readRDS(path), unserialize(serialize(unsupported, NULL)))) {
      before <- serialize(q, NULL)
      for (reunite in list(weasel_apply, weasel_summarize_subset,
                          weasel_selectivity)) {
        out <- serialization_q6_capture(
          error <- tryCatch(reunite(q, "strict", data = d), error = identity)
        )
        expect_length(out$warnings, 0L)
        expect_s3_class(error, "weasel_error_fingerprint_version")
        expect_s3_class(error, "weasel_error")
        expect_match(conditionMessage(error),
                     "unsupported fingerprint encoding version", fixed = TRUE)
        expect_identical(serialize(q, NULL), before)
      }
      # attached-data calls retain their existing guard bypass
      expect_no_warning(weasel_apply(q, "strict"))
    }
  }
})
