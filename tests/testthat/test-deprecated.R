test_that("old names forward to the new ones with a classed warning", {
  d <- make_fixture()
  set_weasel_scope(d, "id", "time", size = 1)
  on.exit(weasel_clear_scope(), add = TRUE)
  evaluate_weasel_scope()

  expect_warning(pv <- reshape_to_wide(), class = "weasel_deprecated")
  expect_identical(pv, weasel:::the$scope$pivot)

  expect_warning(v <- summarize_waves(), class = "weasel_deprecated")
  expect_s3_class(v, "data.frame")

  expect_warning(f <- filter_wave_summary(n_range = c(7, 8)),
                 class = "weasel_deprecated")
  expect_true(all(f$n >= 7))

  expect_warning(s <- get_data_by_row(1), class = "weasel_deprecated")
  expect_gt(nrow(s), 0)

  expect_warning(generate_sets(), class = "weasel_deprecated")
  expect_warning(filter_sets(), class = "weasel_deprecated")

  expect_warning(capture.output(logo()), class = "weasel_deprecated")
})
