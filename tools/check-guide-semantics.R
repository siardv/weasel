# guide semantic parity check
#
# the guide-snippets workflow proves every guide snippet *executes*;
# executability and semantic agreement are different guarantees, and
# this script covers the second one for the recipe where they can
# drift: the bounded-scope recipe filters the pattern table and then
# extracts data, so the pattern it displays and the pattern it
# extracts must be the same pattern. run from the repository root with
# the package installed (CI runs it after the snippet step).

options(weasel.verbose = FALSE)
suppressMessages(library(weasel))

fail <- function(...) {
  cat("SEMANTIC CHECK FAIL:", ..., "\n")
  quit(status = 1L)
}

app <- readLines(file.path("docs", "app.js"), warn = FALSE)

# title greps: the bounded recipe filters on `ids` (respondents per
# pattern), so its displayed title must say ids >= 5; `n` means
# observed waves in the package's own vocabulary
if (!any(grepl("Common patterns (ids >= 5)", app, fixed = TRUE))) {
  fail("bounded-recipe title 'Common patterns (ids >= 5)' not found in docs/app.js")
}
if (any(grepl("Common patterns (n >= 5)", app, fixed = TRUE))) {
  fail("bounded-recipe title mislabels the ids filter as an n filter")
}

# the recipe must extract through the stable pattern id of the named
# filtered table, guarded against the empty-filter case
if (!any(grepl("weasel_get_data_by_row(common$pattern[1])", app, fixed = TRUE))) {
  fail("bounded recipe does not extract via common$pattern[1]")
}
if (!any(grepl("nrow(common) > 0", app, fixed = TRUE))) {
  fail("bounded recipe does not guard the empty filtered table")
}

# execute the recipe's semantics with the CI panel and assert parity:
# the participation string of every extracted respondent must equal
# the first displayed filtered pattern
your_data <- generate_weasel_dummy_data(n_ids = 120, n_times = 10, seed = 99)
set_weasel_scope(your_data, "id", "time", upper = 10)
evaluate_weasel_scope()
weasel_reshape_to_wide()
weasel_summarize_waves()
common <- weasel_filter_wave_summary(ids_range = c(5, Inf))
if (nrow(common) == 0) {
  fail("CI panel produced an empty filtered table; parity cannot be checked")
}
extracted <- weasel_get_data_by_row(common$pattern[1])

span <- 1:10
pattern_of <- function(one_id) {
  w <- sort(unique(extracted$time[extracted$id == one_id]))
  w <- w[w %in% span]
  paste(ifelse(span %in% w, as.character(span), "."), collapse = " ")
}
ids <- unique(extracted$id)
patterns <- vapply(ids, pattern_of, character(1))
if (!all(patterns == common$waves[1])) {
  fail("extracted respondents do not all match the displayed pattern '",
       common$waves[1], "'")
}
weasel_clear_scope()

cat("guide semantics OK: displayed pattern equals extracted pattern;",
    "titles label the filtered column correctly\n")
