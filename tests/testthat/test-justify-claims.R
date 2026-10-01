# generated explanations describe recorded constraints without inferring intent
test_that("no style asserts motive, decision timing, absence of post hoc choices or superiority", {
  d <- make_fixture()
  plans <- list(
    explicit = weasel_plan(d, "id", "time", lower = 3, upper = 6),
    full     = weasel_plan(d, "id", "time", span = "full"),
    core     = weasel_plan(d, "id", "time", span = "core", core_len = 4)
  )
  open <- data.frame(scenario = "open", require_endpoints = FALSE,
                     max_missing = Inf, n_gap_max = Inf, max_gap_len = Inf)
  plans$open <- weasel_plan(d, "id", "time", span = "full", scenarios = open)
  forbidden <- paste0(
    "post hoc|ad hoc|the goal was|improves transparency|superior|",
    "a priori|a-priori|pre-?registered|pre-?specified|design decision"
  )
  for (nm in names(plans)) {
    p <- plans[[nm]]
    sc <- p$plan$scenario[1]
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(p, sc, style = style, cite = FALSE)
      expect_false(grepl(forbidden, txt, ignore.case = TRUE),
                   info = paste(nm, style))
    }
    ext <- weasel_justify_subset(p, sc, style = "extended", cite = FALSE)
    # the supported content survives: explicit definitions and determinism
    expect_match(ext, "defined explicitly", fixed = TRUE)
    expect_match(ext, "fully determined by the declared constraints", fixed = TRUE)
  }
})

test_that("scenario characterizations are attached only to the default rules", {
  d <- make_fixture()
  reused <- data.frame(
    scenario          = c("anchored_balanced", "lenient_info_max", "anchored_strict"),
    require_endpoints = c(FALSE, TRUE, FALSE),
    max_missing = c(3, 0, 4), n_gap_max = c(3, 0, 4), max_gap_len = c(3, 0, 4)
  )
  p <- weasel_plan(d, "id", "time", span = "full", scenarios = reused)
  expect_identical(p$plan$note, c("", "", ""))
  for (sc in p$plan$scenario) {
    for (style in c("methods", "concise", "extended")) {
      txt <- weasel_justify_subset(p, sc, style = style, cite = FALSE)
      expect_false(grepl("characterized as|described as", txt), info = paste(sc, style))
      # the endpoint clause and the (absent) label can no longer disagree
      expect_false(grepl("anchored endpoints|endpoints not guaranteed", txt, fixed = FALSE))
    }
  }
  # the default table keeps its labels and text unchanged
  p0 <- weasel_plan(d, "id", "time", span = "full")
  expect_identical(p0$plan$note,
                   c("cleanest panel, smallest N", "good balance, anchored endpoints",
                     "largest N, endpoints not guaranteed"))
  expect_match(weasel_justify_subset(p0, "anchored_balanced", cite = FALSE),
               "characterized as: good balance, anchored endpoints", fixed = TRUE)
  # compare selection with the fixture's independently known participation patterns
  expected_ids <- list(
    anchored_balanced = c("a1", "a2", "b1", "c1", "d1"),
    lenient_info_max = c("a1", "a2"),
    anchored_strict = c("a1", "a2", "b1", "c1", "d1", "e1")
  )
  expect_identical(stats::setNames(p$plan$ids, p$plan$scenario), expected_ids)
  for (sc in names(expected_ids)) {
    expected_rows <- d[d$id %in% expected_ids[[sc]], , drop = FALSE]
    expect_identical(weasel_apply(p, sc), expected_rows)
  }
})

test_that("explicit copies of default rules receive no default characterizations", {
  d <- make_fixture()
  default_plan <- weasel_plan(d, "id", "time", span = "full")
  null_plan <- weasel_plan(d, "id", "time", span = "full", scenarios = NULL)
  expect_identical(null_plan, default_plan)

  rule_columns <- c("scenario", "require_endpoints", "max_missing",
                    "n_gap_max", "max_gap_len")
  custom_plan <- weasel_plan(d, "id", "time", span = "full",
                             scenarios = default_plan$plan[rule_columns])
  expect_identical(custom_plan$plan$note, rep("", 3))
  # every selection, metric and recorded field still matches the default plan
  default_plan$plan$note <- rep("", 3)
  expect_identical(custom_plan, default_plan)
})

test_that("saved plans retain their stored scenario characterizations", {
  d <- make_fixture()
  rules <- data.frame(
    scenario = "anchored_balanced", require_endpoints = FALSE,
    max_missing = 3, n_gap_max = 3, max_gap_len = 3
  )
  p <- weasel_plan(d, "id", "time", span = "full", scenarios = rules)
  # represent a note already stored by an earlier package version
  p$plan$note <- "good balance, anchored endpoints"
  saved_path <- tempfile(fileext = ".rds")
  on.exit(unlink(saved_path), add = TRUE)
  saveRDS(p, saved_path)
  saved_plan <- readRDS(saved_path)
  expect_identical(saved_plan, p)

  for (style in c("methods", "extended")) {
    txt <- weasel_justify_subset(saved_plan, "anchored_balanced",
                                 style = style, cite = FALSE)
    expect_match(txt, "good balance, anchored endpoints", fixed = TRUE)
  }
  expected_rows <- d[d$id %in% c("a1", "a2", "b1", "c1", "d1"), , drop = FALSE]
  expect_identical(weasel_apply(saved_plan, "anchored_balanced"), expected_rows)
  expect_identical(saved_plan, p)
})
