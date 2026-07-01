#' Print a data frame as a formatted table
#'
#' The standard display function used at every stage of both pipelines.
#' Output is produced with base R only, so it is identical regardless of
#' which optional packages are installed. List columns (such as the
#' per-scenario `ids` column of a plan table) are hidden from display
#' and reported by name instead.
#'
#' @param x A data frame to print.
#' @param title Optional character string displayed as a heading above
#'   the table.
#' @param digits Integer; number of decimal places for numeric columns.
#' @param n Optional integer; maximum number of rows to show.
#'
#' @return Invisibly returns the displayed (possibly truncated and
#'   rounded) data frame, without hidden list columns.
#'
#' @examples
#' df <- data.frame(a = 1:5, b = rnorm(5))
#' weasel_print_table(df, title = "Example", digits = 2, n = 3)
#'
#' # typical use: display scenario comparison from the plan pipeline
#' d <- generate_weasel_dummy_data(n_ids = 60, n_times = 8, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' weasel_print_table(weasel_compare_scenarios(p), title = "Scenarios")
#'
#' @export
weasel_print_table <- function(x, title = NULL, digits = 3, n = NULL) {
  if (!inherits(x, "data.frame")) .weasel_stop("x must be a data.frame.")
  digits <- suppressWarnings(as.integer(digits[1]))
  if (is.na(digits) || digits < 0) {
    .weasel_stop("digits must be a single non-negative integer.")
  }
  if (!is.null(n)) {
    n <- suppressWarnings(as.integer(n[1]))
    if (is.na(n) || n < 0) {
      .weasel_stop("n must be a single non-negative integer.")
    }
  }

  if (!is.null(title)) .weasel_h2(title)

  is_list_col <- vapply(x, is.list, logical(1))
  y <- x[!is_list_col]
  for (nm in names(y)) y[[nm]] <- .weasel_maybe_round(y[[nm]], digits = digits)

  if (!is.null(n)) {
    y <- y[seq_len(min(nrow(y), n)), , drop = FALSE]
  }

  print(y, row.names = FALSE)
  if (any(is_list_col)) {
    cat("(list column(s) not shown: ",
        paste(names(x)[is_list_col], collapse = ", "), ")\n", sep = "")
  }
  cat("\n")
  invisible(y)
}
