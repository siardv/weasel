#' Run a full weasel demonstration
#'
#' Exercises both the scope-based pipeline and the planning interface on
#' synthetic dummy data, including the sensitivity diagnostic, then
#' cleans up the scope. Display is suppressible with `suppressMessages()`
#' or `options(weasel.verbose = FALSE)`; results are still returned.
#'
#' @param seed Random seed passed to [generate_weasel_dummy_data()].
#'
#' @return A list with elements `data`, `plan`, `compare`, `summary`,
#'   and `sensitivity`, invisibly.
#'
#' @examples
#' res <- weasel_example(seed = 42)
#'
#' @export
weasel_example <- function(seed = 42) {
  .weasel_h2(weasel_text(post = " example"))
  long_data <- generate_weasel_dummy_data(seed = seed)
  on.exit(weasel_clear_scope(), add = TRUE)

  set_weasel_scope(long_data, id = "id", wave = "time", max_gap_len = 2)
  evaluate_weasel_scope()
  weasel_reshape_to_wide()
  weasel_summarize_waves()
  weasel_scope_info()

  .weasel_example_table(weasel_filter_wave_summary(),
                     title = "Wave pattern summary (top 10)", n = 10)

  subset_from_row1 <- weasel_get_data_by_row(1)
  .weasel_example_table(utils::head(subset_from_row1, 10),
                     title = "Subset from row 1 (preview)", n = 10)

  plan_obj <- weasel_plan(long_data, id = "id", wave = "time", span = "core")

  cmp <- weasel_compare_scenarios(plan_obj)
  .weasel_example_table(cmp, title = "Scenario comparison", digits = 3)
  .weasel_msg(weasel_compare_to_sentence(cmp))

  s <- weasel_summarize_subset(plan_obj, "anchored_balanced")
  .weasel_example_table(s$headline, title = "Chosen subset headline", digits = 3)
  .weasel_msg(weasel_subset_to_sentence(s))

  sens <- weasel_sensitivity(plan_obj, max_missing = 0:2)
  .weasel_example_table(sens, title = "Tolerance sensitivity (top rows)", n = 6)

  .weasel_h2("Justification paragraph (methods style)")
  just <- weasel_justify_subset(plan_obj, "anchored_balanced")
  .weasel_msg(just)

  invisible(list(data = long_data, plan = plan_obj,
                 compare = cmp, summary = s, sensitivity = sens))
}

# display demonstration tables as messages while keeping their return values
.weasel_example_table <- function(...) {
  output <- utils::capture.output(value <- weasel_print_table(...))
  if (length(output)) .weasel_msg(paste(output, collapse = "\n"))
  invisible(value)
}
