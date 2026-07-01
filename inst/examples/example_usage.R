# weasel: example usage
# demonstrates both pipelines on synthetic panel data

library(weasel)

# 1. generate panel data with realistic wave-level missingness
#    (a missed wave is an absent row, as in real long-format files)
long_data <- generate_weasel_dummy_data(
  n_ids = 500,
  n_times = 12,
  seed = 42
)

# 2. scope pipeline: explore wave patterns interactively
set_weasel_scope(
  long_data,
  id = "id",
  wave = "time",
  lower = 2,
  upper = 11,
  gap = 2      # drop respondents with any interior gap longer than 2 waves
)
evaluate_weasel_scope()
weasel_reshape_to_wide()
weasel_summarize_waves()

# inspect patterns: n = observed waves in the pattern, ids = respondents
weasel_print_table(weasel_filter_wave_summary(), title = "All patterns", n = 15)
weasel_print_table(
  weasel_filter_wave_summary(n_range = c(8, 10), ids_range = c(5, Inf)),
  title = "Dense patterns shared by 5+ respondents"
)

# pull the long-format rows behind the most common pattern
subset_row1 <- weasel_get_data_by_row(1)
weasel_print_table(head(subset_row1, 10), title = "Row 1 subset preview")

weasel_clear_scope()

# 3. plan pipeline: compare selection scenarios
plan_obj <- weasel_plan(long_data, id = "id", wave = "time", span = "core")

comparison <- weasel_compare_scenarios(plan_obj)
weasel_print_table(comparison, title = "Scenario comparison", digits = 3)
cat(weasel_compare_to_sentence(comparison), "\n\n")

# audit one scenario in detail before committing
s <- weasel_summarize_subset(plan_obj, "anchored_balanced")
weasel_print_table(s$headline, title = "Headline")
weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")
cat(weasel_subset_to_sentence(s), "\n\n")

# 4. extract the analysis-ready subset
analysis_data <- weasel_apply(plan_obj, "anchored_balanced")
str(analysis_data)

# 5. generate methods-section text
cat(weasel_justify_subset(plan_obj, "anchored_balanced"), "\n")
