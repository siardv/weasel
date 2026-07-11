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
