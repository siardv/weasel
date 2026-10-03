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
  p$fingerprint <- weasel:::.weasel_data_fingerprint(
    fp_d1(), "id", "time", encoding_version = 1L
  )
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

test_that("the v1 fingerprint keeps its saved representation", {
  # fixed digest of the canonical v1 bytes for A: 1,2,3 and B: 1,2
  expect_identical(
    weasel:::.weasel_data_fingerprint(fp_d1(), "id", "time",
                                     encoding_version = 1L),
    list(n_rows = 5L, n_pairs = 5L, n_ids = 2L, id_type = "character",
         waves = 1:3, pairs_per_wave = c(2L, 2L, 1L),
         pair_hash = "bd26387f0367c182b8b6c7a36377e3d2")
  )
})

test_that("saved plans detect changed assignments outside their window", {
  d <- rbind(fp_d1(), data.frame(id = "B", time = 5, var1 = 22,
                                 stringsAsFactors = FALSE))
  p <- suppressMessages(
    weasel_plan(d, "id", "time", lower = 1, upper = 3,
                scenarios = fp_strict(), keep_data = FALSE)
  )
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(p, f)
  p <- readRDS(f)

  changed <- d
  changed$id[6] <- "A"
  changed_fp <- weasel:::.weasel_data_fingerprint(changed, "id", "time")
  count_fields <- setdiff(names(p$fingerprint), "pair_hash")
  expect_identical(p$fingerprint[count_fields], changed_fp[count_fields])
  expect_false(identical(p$fingerprint$pair_hash, changed_fp$pair_hash))

  legacy_counts <- p
  legacy_counts$fingerprint$pair_hash <- NULL
  no_fingerprint <- p
  no_fingerprint$fingerprint <- NULL
  for (reunite in list(weasel_apply, weasel_summarize_subset,
                      weasel_selectivity)) {
    expect_no_warning(expected <- reunite(p, "strict", data = d))
    expect_warning(
      actual <- reunite(p, "strict", data = changed),
      "pair digest mismatch", class = "weasel_data_mismatch"
    )
    # the guard sees the full panel even though the selected output is unchanged
    expect_identical(actual, expected)
    for (legacy in list(legacy_counts, no_fingerprint)) {
      expect_no_warning(actual <- reunite(legacy, "strict", data = changed))
      expect_identical(actual, expected)
    }
  }
})

test_that("row counts and id types guard reunion even with an unchanged digest", {
  d <- rbind(fp_d1(), data.frame(id = "B", time = 5, var1 = 22,
                                 stringsAsFactors = FALSE))
  p <- suppressMessages(
    weasel_plan(d, "id", "time", lower = 1, upper = 3,
                scenarios = fp_strict(), keep_data = FALSE)
  )
  d_factor <- d
  d_factor$id <- factor(d_factor$id, levels = c("B", "A"))
  variants <- list(
    duplicate = rbind(d, d[6, , drop = FALSE]),
    missing_key = rbind(d, data.frame(id = NA_character_, time = NA_real_,
                                      var1 = NA_real_)),
    factor_id = d_factor
  )
  changed_fields <- c(duplicate = "n_rows", missing_key = "n_rows",
                      factor_id = "id_type")
  legacy_counts <- p
  legacy_counts$fingerprint$pair_hash <- NULL
  no_fingerprint <- p
  no_fingerprint$fingerprint <- NULL

  for (variant in names(variants)) {
    changed <- variants[[variant]]
    changed_fp <- weasel:::.weasel_data_fingerprint(changed, "id", "time")
    field <- changed_fields[[variant]]
    unchanged <- setdiff(names(p$fingerprint), field)
    expect_identical(p$fingerprint[unchanged], changed_fp[unchanged],
                     info = variant)
    expect_false(identical(p$fingerprint[[field]], changed_fp[[field]]),
                 info = variant)

    for (reunite in list(weasel_apply, weasel_summarize_subset,
                        weasel_selectivity)) {
      expect_no_warning(expected <- reunite(no_fingerprint, "strict",
                                             data = changed))
      for (guarded in list(p, legacy_counts)) {
        expect_warning(
          actual <- reunite(guarded, "strict", data = changed),
          class = "weasel_data_mismatch"
        )
        expect_identical(actual, expected, info = variant)
      }
    }
  }
})

test_that("new fingerprints record the exact v2 format", {
  # independently framed UTF-8 bytes for A: 1,2,3 and B: 1,2
  expect_identical(
    weasel:::.weasel_data_fingerprint(fp_d1(), "id", "time"),
    list(n_rows = 5L, n_pairs = 5L, n_ids = 2L, id_type = "character",
         waves = 1:3, pairs_per_wave = c(2L, 2L, 1L),
         pair_hash = "025e7633f42330f4a2e21bea611719b9",
         encoding_version = 2L)
  )
  # exact binary64 encodings, with equality's common zero identity
  expect_identical(
    weasel:::.weasel_id_key(c(1, 1 + .Machine$double.eps, 0, -0)),
    c("n3ff0000000000000", "n3ff0000000000001",
      "n0000000000000000", "n0000000000000000")
  )
})

test_that("v2 distinguishes exact numeric and time assignment changes", {
  values <- list(
    epsilon = c(1, 1 + .Machine$double.eps),
    large = c(1e15, 1e15 + 1),
    complex = complex(real = c(1, 1 + .Machine$double.eps), imaginary = 2),
    date = structure(c(1, 1 + .Machine$double.eps), class = "Date"),
    time = structure(c(1, 1 + .Machine$double.eps),
                     class = c("POSIXct", "POSIXt"), tzone = "UTC")
  )
  for (value in values) {
    d <- data.frame(id = value[c(1, 1, 1, 2, 2)], time = c(1, 2, 3, 1, 2))
    changed <- d
    changed$id[3] <- value[2]
    before <- weasel:::.weasel_data_fingerprint(d, "id", "time")
    after <- weasel:::.weasel_data_fingerprint(changed, "id", "time")
    expect_identical(before[setdiff(names(before), "pair_hash")],
                     after[setdiff(names(after), "pair_hash")])
    expect_false(identical(before$pair_hash, after$pair_hash))
    # historical fingerprints retain precisely their original weakness
    expect_identical(
      weasel:::.weasel_data_fingerprint(d, "id", "time", 1L),
      weasel:::.weasel_data_fingerprint(changed, "id", "time", 1L)
    )
  }
})

test_that("v2 frames separator-containing character IDs unambiguously", {
  left <- data.frame(id = c("A\x1f2\x1eB", "C", "D"), time = c(2L, 3L, 4L))
  right <- data.frame(id = c("A", "B\x1f2\x1eC", "D"), time = c(2L, 3L, 4L))
  expect_identical(
    weasel:::.weasel_data_fingerprint(left, "id", "time", 1L),
    weasel:::.weasel_data_fingerprint(right, "id", "time", 1L)
  )
  expect_false(identical(
    weasel:::.weasel_data_fingerprint(left, "id", "time")$pair_hash,
    weasel:::.weasel_data_fingerprint(right, "id", "time")$pair_hash
  ))
  for (ids in list(c("", ":;\x1e\x1f"), c("1", "01"))) {
    expect_length(unique(weasel:::.weasel_id_key(ids)), 2L)
  }
})

test_that("v2 preserves supported equality and ignores display options", {
  original_options <- options()
  on.exit(options(original_options), add = TRUE)
  d <- data.frame(id = c(1e-7, 1.25, 1e15, Inf, -Inf), time = 1:5)
  expected <- weasel:::.weasel_data_fingerprint(d, "id", "time")
  for (settings in list(list(scipen = 999), list(OutDec = ","),
                       list(digits = 3), list(scipen = -9, digits = 22))) {
    options(settings)
    expect_identical(weasel:::.weasel_data_fingerprint(d, "id", "time"),
                     expected)
  }
  options(original_options)
  hash <- function(ids) {
    weasel:::.weasel_data_fingerprint(
      data.frame(id = ids, time = seq_along(ids)), "id", "time"
    )$pair_hash
  }
  expect_identical(hash(1:3), hash(as.double(1:3)))
  expect_identical(hash(c(0, 1)), hash(c(-0, 1)))
  expect_identical(hash(complex(real = c(0, 1), imaginary = c(-0, 2))),
                   hash(complex(real = c(-0, 1), imaginary = c(0, 2))))
  expect_identical(hash(structure(1:3, class = "Date")),
                   hash(structure(as.double(1:3), class = "Date")))
  instants <- structure(c(1, 1.25, 2), class = c("POSIXct", "POSIXt"),
                        tzone = "UTC")
  other_zone <- instants
  attr(other_zone, "tzone") <- "Europe/Amsterdam"
  expect_identical(hash(instants), hash(other_zone))
  expect_identical(weasel:::.weasel_id_key(c(TRUE, FALSE)), c("l1", "l0"))
  # empty/missing pairs have one stream regardless of underlying ID type
  for (ids in list(character(), integer(), numeric(), logical(), complex())) {
    expect_identical(hash(ids), "6abfd28ec19277783ec2b0562fde8506")
  }
  missing <- data.frame(id = c(NA_real_, NaN, 1), time = c(1, 2, NA))
  expect_identical(weasel:::.weasel_data_fingerprint(missing, "id", "time")$pair_hash,
                   hash(numeric()))
})

test_that("v2 honors character encoding identity across tested locales", {
  original_locale <- Sys.getlocale("LC_CTYPE")
  original_collation <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_CTYPE", original_locale), add = TRUE)
  on.exit(Sys.setlocale("LC_COLLATE", original_collation), add = TRUE)
  utf8 <- "\u00e9"
  Encoding(utf8) <- "UTF-8"
  latin1 <- rawToChar(as.raw(233))
  Encoding(latin1) <- "latin1"
  bytes <- utf8
  Encoding(bytes) <- "bytes"
  hash <- function(id) {
    weasel:::.weasel_data_fingerprint(data.frame(id = id, time = 1L),
                                     "id", "time")$pair_hash
  }
  expected <- hash(utf8)
  for (locale in c("C", "C.UTF-8", "en_US.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", locale)) == "") next
    suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    expect_identical(hash(utf8), expected)
    expect_identical(hash(latin1), expected)
    expect_false(identical(hash(bytes), expected))
    invalid <- rawToChar(as.raw(195))
    # no accepted native byte string may become a literal display escape
    expect_false(identical(hash(invalid), hash("<c3>")))
    marked_invalid <- invalid
    Encoding(marked_invalid) <- "UTF-8"
    expect_false(identical(hash(marked_invalid), hash("<c3>")))
    expect_identical(identical(hash(invalid), hash(marked_invalid)),
                     isTRUE(invalid == marked_invalid))
  }
})

test_that("v2 normalizes participation keys and excludes covariate values", {
  d <- fp_d1()
  original <- weasel:::.weasel_data_fingerprint(d, "id", "time")
  near <- d
  near$time <- near$time + 1e-9
  near$var1 <- rev(near$var1)
  expect_identical(weasel:::.weasel_data_fingerprint(near, "id", "time"),
                   original)
  with_missing <- rbind(d, data.frame(id = NA_character_, time = 9, var1 = 0))
  changed_missing <- with_missing
  changed_missing$time[6] <- 15
  expect_identical(
    weasel:::.weasel_data_fingerprint(with_missing, "id", "time"),
    weasel:::.weasel_data_fingerprint(changed_missing, "id", "time")
  )
  expect_no_warning(weasel_apply(fp_plan(), "strict", data = near))
})
