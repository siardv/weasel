# regression tests for the pair-level data fingerprint (0.4.1): the
# reunion guard must detect any change to the deduplicated (id, wave)
# incidence structure, not only changes to the aggregate counts that
# two different panels can share

# the adjustment report's counterexample: participation swapped between
# ids while every aggregate count stays identical
fp_d1 <- function() {
  data.frame(id = c("A", "A", "A", "B", "B"),
             time = c(1, 2, 3, 1, 2),
             var1 = c(10, 11, 12, 20, 21),
             stringsAsFactors = FALSE)
}
fp_d2 <- function() {
  data.frame(id = c("A", "A", "B", "B", "B"),
             time = c(1, 2, 1, 2, 3),
             var1 = c(10, 11, 20, 21, 22),
             stringsAsFactors = FALSE)
}
fp_strict <- function() {
  data.frame(scenario = "strict", require_endpoints = TRUE,
             max_missing = 0, n_gap_max = 0, max_gap_len = 0,
             stringsAsFactors = FALSE)
}
fp_plan <- function(d = fp_d1()) {
  suppressMessages(
    weasel_plan(d, "id", "time", span = "full", scenarios = fp_strict(),
                keep_data = FALSE)
  )
}

test_that("fingerprints carry a pair digest alongside the counts", {
  f <- weasel:::.weasel_data_fingerprint(fp_d1(), "id", "time")
  expect_true(is.character(f$pair_hash))
  expect_length(f$pair_hash, 1L)
  expect_identical(nchar(f$pair_hash), 32L)
  # the descriptive counts remain, so mismatch warnings stay informative
  expect_identical(f$n_rows, 5L)
  expect_identical(f$n_pairs, 5L)
  expect_identical(f$n_ids, 2L)
})

test_that("the pair digest is row-order and duplicate invariant", {
  d <- fp_d1()
  f0 <- weasel:::.weasel_data_fingerprint(d, "id", "time")
  f_rev <- weasel:::.weasel_data_fingerprint(
    d[rev(seq_len(nrow(d))), , drop = FALSE], "id", "time"
  )
  f_dup <- weasel:::.weasel_data_fingerprint(
    rbind(d, d[2, , drop = FALSE]), "id", "time"
  )
  expect_true(is.character(f0$pair_hash))
  expect_identical(f0$pair_hash, f_rev$pair_hash)
  expect_identical(f0$pair_hash, f_dup$pair_hash)
})

test_that("the pair digest is invariant to the id representation", {
  d_chr <- fp_d1()
  d_fac <- fp_d1()
  d_fac$id <- factor(d_fac$id, levels = c("B", "A"))
  f_chr <- weasel:::.weasel_data_fingerprint(d_chr, "id", "time")
  f_fac <- weasel:::.weasel_data_fingerprint(d_fac, "id", "time")
  expect_true(is.character(f_chr$pair_hash))
  expect_identical(f_chr$pair_hash, f_fac$pair_hash)
})

test_that("reordered rows of the same panel reunite without a warning", {
  p <- fp_plan()
  d_perm <- fp_d1()[c(4, 1, 5, 3, 2), , drop = FALSE]
  expect_no_warning(weasel_apply(p, "strict", data = d_perm))
})

test_that("duplicated pairs alone do not trigger a data mismatch", {
  p <- fp_plan()
  d_dup <- rbind(fp_d1(), fp_d1()[2, , drop = FALSE])
  # note: n_rows changes, so compare digests directly instead of the
  # reunion warning (participation is deduplicated before hashing)
  f0 <- weasel:::.weasel_data_fingerprint(fp_d1(), "id", "time")
  f1 <- weasel:::.weasel_data_fingerprint(d_dup, "id", "time")
  expect_identical(f0$pair_hash, f1$pair_hash)
  expect_true(is.character(f0$pair_hash))
  expect_false(identical(f0$n_rows, f1$n_rows))
  expect_true(!is.null(p))
})

test_that("swapped participation with identical counts warns on reunion", {
  p <- fp_plan()
  expect_warning(
    weasel_apply(p, "strict", data = fp_d2()),
    class = "weasel_data_mismatch"
  )
})

test_that("the pure-assignment mismatch message names the digest, not counts", {
  p <- fp_plan()
  w <- tryCatch(
    {
      weasel_apply(p, "strict", data = fp_d2())
      NULL
    },
    warning = function(w) w
  )
  expect_true(!is.null(w))
  expect_match(conditionMessage(w), "assignments differ",
               fixed = TRUE)
})

test_that("an id renamed while counts stay constant warns on reunion", {
  p <- fp_plan()
  d3 <- fp_d1()
  d3$id[d3$id == "B"] <- "C"
  expect_warning(
    weasel_apply(p, "strict", data = d3),
    class = "weasel_data_mismatch"
  )
})

test_that("wave reassignment with constant per-wave counts warns", {
  # A: 1,2,4 / B: 1,3,4 versus A: 1,3,4 / B: 1,2,4; per-wave counts and
  # per-id counts are identical, only the incidence structure differs
  e1 <- data.frame(id = c("A", "A", "A", "B", "B", "B"),
                   time = c(1, 2, 4, 1, 3, 4),
                   stringsAsFactors = FALSE)
  e2 <- data.frame(id = c("A", "A", "A", "B", "B", "B"),
                   time = c(1, 3, 4, 1, 2, 4),
                   stringsAsFactors = FALSE)
  lenient <- data.frame(scenario = "any", require_endpoints = FALSE,
                        max_missing = Inf, n_gap_max = Inf,
                        max_gap_len = Inf, stringsAsFactors = FALSE)
  p <- suppressMessages(
    weasel_plan(e1, "id", "time", span = "full", scenarios = lenient,
                keep_data = FALSE)
  )
  expect_no_warning(weasel_apply(p, "any", data = e1))
  expect_warning(
    weasel_apply(p, "any", data = e2),
    class = "weasel_data_mismatch"
  )
})

test_that("all three reunion paths use the strengthened comparison", {
  d1 <- fp_d1()
  d2 <- fp_d2()
  p <- suppressMessages(
    weasel_plan(d1, "id", "time", span = "full", scenarios = fp_strict(),
                keep_data = FALSE)
  )
  expect_warning(
    weasel_apply(p, "strict", data = d2),
    class = "weasel_data_mismatch"
  )
  expect_warning(
    weasel_summarize_subset(p, "strict", data = d2),
    class = "weasel_data_mismatch"
  )
  expect_warning(
    weasel_selectivity(p, "strict", vars = "var1", data = d2),
    class = "weasel_data_mismatch"
  )
})

test_that("count changes still warn and report the counts", {
  p <- fp_plan()
  d_less <- fp_d1()[-1, , drop = FALSE]
  w <- tryCatch(
    {
      weasel_apply(p, "strict", data = d_less)
      NULL
    },
    warning = function(w) w
  )
  expect_true(!is.null(w))
  expect_s3_class(w, "weasel_data_mismatch")
  expect_match(conditionMessage(w), "rows 5 -> 4", fixed = TRUE)
})

test_that("the fingerprint and its digest survive serialization", {
  p <- fp_plan()
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(p, f)
  p2 <- readRDS(f)
  expect_true(is.character(p2$fingerprint$pair_hash))
  expect_identical(p2$fingerprint$pair_hash, p$fingerprint$pair_hash)
  expect_no_warning(weasel_apply(p2, "strict", data = fp_d1()))
  expect_warning(
    weasel_apply(p2, "strict", data = fp_d2()),
    class = "weasel_data_mismatch"
  )
})

test_that("legacy plans without a pair digest keep the documented behavior", {
  # a plan whose stored fingerprint predates pair_hash: only the fields
  # the stored fingerprint carries are compared
  p <- fp_plan()
  p$fingerprint$pair_hash <- NULL
  expect_no_warning(weasel_apply(p, "strict", data = fp_d1()))
  # count-identical swapped data passed a 0.4.0 fingerprint, and must
  # keep passing for legacy plans
  expect_no_warning(weasel_apply(p, "strict", data = fp_d2()))
  # count changes still warn for legacy plans
  expect_warning(
    weasel_apply(p, "strict", data = fp_d1()[-1, , drop = FALSE]),
    class = "weasel_data_mismatch"
  )
})

test_that("plans without any fingerprint are accepted silently", {
  p <- fp_plan()
  p$fingerprint <- NULL
  expect_no_warning(weasel_apply(p, "strict", data = fp_d2()))
})

test_that("attached-data workflows never consult the fingerprint", {
  # the guard applies only to explicitly supplied data; the attached
  # path cannot mismatch by construction
  p_full <- suppressMessages(
    weasel_plan(fp_d1(), "id", "time", span = "full",
                scenarios = fp_strict())
  )
  expect_no_warning(weasel_apply(p_full, "strict"))
  expect_no_warning(weasel_summarize_subset(p_full, "strict"))
})
