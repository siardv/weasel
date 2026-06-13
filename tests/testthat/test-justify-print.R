test_that("justification text is a single string with key figures", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")

  txt <- weasel_justify_subset(p, "anchored_balanced")
  expect_type(txt, "character")
  expect_length(txt, 1)

  n <- p$plan$n_ids[p$plan$scenario == "anchored_balanced"]
  expect_match(txt, paste0("retained ", n, " respondent"))
  expect_match(txt, "waves 1 to 8")
  expect_match(txt, "interior missing block")

  # the three styles produce distinct paragraphs
  styles <- c("methods", "concise", "extended")
  outs <- vapply(styles, function(s) {
    weasel_justify_subset(p, "anchored_balanced", style = s)
  }, character(1))
  expect_equal(length(unique(outs)), 3)

  # citation handling
  with_author <- weasel_justify_subset(p, "anchored_balanced",
                                       author = "Doe", year = "2026")
  expect_match(with_author, "(Doe, 2026)", fixed = TRUE)
  no_cite <- weasel_justify_subset(p, "anchored_balanced", cite = FALSE)
  expect_false(grepl("R package weasel", no_cite, fixed = TRUE))

  expect_error(weasel_justify_subset(p, "nonexistent"), "not found")
})

test_that("weasel_print_table hides list columns, rounds, and truncates", {
  df <- data.frame(a = c(1.23456, 2.34567, 3.45678), b = c("x", "y", "z"))
  df$lst <- list(1:2, 3:4, 5:6)

  out_txt <- capture.output(res <- weasel_print_table(df, digits = 2, n = 2))
  expect_false("lst" %in% names(res))
  expect_equal(nrow(res), 2)
  expect_equal(res$a, c(1.23, 2.35))
  expect_true(any(grepl("list column", out_txt)))

  expect_error(weasel_print_table(1:3), "data.frame")
})

test_that("compare sentence mentions the recommended scenario", {
  d <- make_fixture()
  p <- weasel_plan(d, "id", "time", span = "full")
  cmp <- weasel_compare_scenarios(p)
  s <- weasel_compare_to_sentence(cmp)
  rec <- cmp$scenario[cmp$recommended]
  expect_match(s, rec, fixed = TRUE)
  expect_match(s, "Recommended scenario")
})

test_that("scenario matching accepts unambiguous abbreviations only", {
  choices <- c("anchored_strict", "anchored_balanced", "lenient_info_max")
  expect_equal(weasel_match_scenario("lenient", choices), "lenient_info_max")
  expect_equal(weasel_match_scenario("anchored_strict", choices),
               "anchored_strict")
  expect_error(weasel_match_scenario("anchored", choices), "ambiguous")
})
