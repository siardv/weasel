# cosmetic colour sampling: rotation, caller hygiene, exclusions

test_that("draws differ even when the caller's seed is fixed (frozen-logo regression)", {
  set.seed(7)
  a <- replicate(4, paste(weasel:::sample_colors(), collapse = "-"))
  set.seed(7)
  b <- replicate(4, paste(weasel:::sample_colors(), collapse = "-"))
  expect_gt(length(unique(c(a, b))), 1)
})

test_that("colour draws rotate across consecutive calls", {
  draws <- replicate(5, paste(weasel:::sample_colors(), collapse = "-"))
  expect_gt(length(unique(draws)), 1)
})

test_that("cosmetic sampling leaves the caller's rng stream untouched", {
  set.seed(42)
  ref <- stats::runif(3)
  set.seed(42)
  invisible(weasel:::sample_colors())
  invisible(weasel:::sample_colors())
  expect_identical(stats::runif(3), ref)

  # and it does not materialise a seed in a session that has none
  rm(".Random.seed", envir = globalenv())
  invisible(weasel:::sample_colors())
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("exclusions hold and the full palette is returned", {
  for (i in 1:5) {
    cols <- weasel:::sample_colors("green")
    expect_setequal(cols, weasel:::.weasel_palette)
    expect_false(cols[1] == "green")
  }
})
