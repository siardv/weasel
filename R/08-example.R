#' Run a full weasel demonstration
#'
#' Exercises both the scope-based pipeline and the planning interface on
#' synthetic dummy data, then cleans up the scope.
#'
#' @param seed Random seed passed to [generate_weasel_dummy_data()].
#'
#' @return A list with elements `data`, `plan`, `compare`, and
#'   `summary`, invisibly.
#'
#' @examples
#' \dontrun{
#' res <- weasel_example(seed = 42)
#' }
#'
#' @export
weasel_example <- function(seed = 42) {
  .weasel_h2(weasel_text(post = " example"))
  long_data <- generate_weasel_dummy_data(seed = seed)
  on.exit(weasel_clear_scope(), add = TRUE)

  set_weasel_scope(long_data, id = "id", wave = "time", gap = 2)
  evaluate_weasel_scope()
  reshape_to_wide()
  summarize_waves()

  weasel_print_table(filter_wave_summary(),
                     title = "Wave pattern summary (top 10)", n = 10)

  subset_from_row1 <- get_data_by_row(1)
  weasel_print_table(utils::head(subset_from_row1, 10),
                     title = "Subset from row 1 (preview)", n = 10)

  plan_obj <- weasel_plan(long_data, id = "id", wave = "time", span = "core")

  cmp <- weasel_compare_scenarios(plan_obj)
  weasel_print_table(cmp, title = "Scenario comparison", digits = 3)
  .weasel_msg(weasel_compare_to_sentence(cmp))

  s <- weasel_summarize_subset(plan_obj, "anchored_balanced")
  weasel_print_table(s$headline, title = "Chosen subset headline", digits = 3)
  .weasel_msg(weasel_subset_to_sentence(s))

  .weasel_h2("Justification paragraph (methods style)")
  just <- weasel_justify_subset(plan_obj, "anchored_balanced")
  .weasel_msg(just)

  invisible(list(data = long_data, plan = plan_obj,
                 compare = cmp, summary = s))
}
