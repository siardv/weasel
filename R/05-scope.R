# scope-based interactive pipeline
# state lives in the package-internal environment `the` (see 01-utils.R),
# never in the user's global environment

#' Set up a weasel scope for wave-pattern analysis
#'
#' Initialises the package-internal scope that holds data, column names,
#' and wave-range parameters used by the downstream pipeline functions.
#' All arguments are validated immediately, so mistakes fail here rather
#' than several steps later. The user's global environment is never
#' modified.
#'
#' @param data A data frame in long format. A respondent is considered
#'   observed at a wave if a row with that (id, wave) pair exists.
#' @param id Name of the respondent-identifier column. Any atomic type
#'   (integer, character, ...) is supported.
#' @param wave Name of the wave/time column. Must be numeric with
#'   integer-valued entries.
#' @param size Optional integer vector of acceptable per-respondent
#'   observation counts; respondents with fewer than `min(size)`
#'   observed waves are dropped by [weasel_reshape_to_wide()]. Defaults
#'   to `min(3, span length)` through the span length.
#' @param lower Optional lower bound of the wave range.
#' @param upper Optional upper bound of the wave range.
#' @param gap Optional integer; maximum allowed length of an interior
#'   gap (a run of missing waves strictly between a respondent's first
#'   and last observed wave inside the span). `NULL` (default) applies
#'   no constraint.
#' @param n_gap Optional integer; maximum allowed number of interior
#'   gaps. `NULL` (default) applies no constraint.
#' @param grid How the wave grid inside the span is defined.
#'   `"consecutive"` (default) treats every integer between `lower` and
#'   `upper` as a scheduled wave; `"observed"` uses only wave values
#'   that occur anywhere in the data, which is the right choice for
#'   biennial and other non-consecutive schedules. With
#'   `"consecutive"`, [evaluate_weasel_scope()] warns when the span
#'   contains waves that no respondent has.
#' @param override If `TRUE`, overwrite any existing scope. If `FALSE`
#'   and a scope already exists, an error is raised.
#'
#' @return The scope environment, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#'
#' # step 1 of the scope pipeline: bind data and define the wave range
#' set_weasel_scope(d, "id", "time", upper = 8)
#'
#' # only respondents whose interior gaps are at most 1 wave long
#' set_weasel_scope(d, "id", "time", gap = 1)
#'
#' # biennial schedule: use the observed grid
#' b <- generate_weasel_dummy_data(n_ids = 30, waves = seq(2008, 2018, 2),
#'                                 seed = 1)
#' set_weasel_scope(b, "id", "time", grid = "observed")
#'
#' weasel_clear_scope()
#'
#' @export
set_weasel_scope <- function(data,
                             id,
                             wave,
                             size = NULL,
                             lower = NULL,
                             upper = NULL,
                             gap = NULL,
                             n_gap = NULL,
                             grid = c("consecutive", "observed"),
                             override = TRUE) {
  .weasel_check_id_wave(data, id, wave)
  grid  <- match.arg(grid)
  lower <- .weasel_check_bound(lower, "lower")
  upper <- .weasel_check_bound(upper, "upper")
  if (!is.null(lower) && !is.null(upper) && upper < lower) {
    .weasel_stop("upper must be >= lower.")
  }
  gap   <- .weasel_check_count(gap, "gap")
  n_gap <- .weasel_check_count(n_gap, "n_gap")
  if (!is.null(size)) {
    size <- suppressWarnings(as.integer(size))
    if (length(size) == 0 || anyNA(size) || any(size < 1)) {
      .weasel_stop("size must be a vector of positive integers.")
    }
  }

  if (!is.null(the$scope) && !isTRUE(override)) {
    .weasel_stop("a scope already exists; set override = TRUE to replace it.")
  }

  env <- new.env(parent = emptyenv())
  env$data  <- data
  env$id    <- id
  env$wave  <- wave
  env$size  <- size
  env$lower <- lower
  env$upper <- upper
  env$gap   <- gap
  env$n_gap <- n_gap
  env$grid  <- grid

  the$scope <- env
  .weasel_ok(weasel_text(post = " scope set"))
  invisible(env)
}

#' Clear the active weasel scope
#'
#' Removes the scope created by [set_weasel_scope()]. Useful at the end
#' of scripts, examples, and tests.
#'
#' @return `TRUE` invisibly if a scope was removed, `FALSE` otherwise.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 20, n_times = 5, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' weasel_clear_scope()
#'
#' @export
weasel_clear_scope <- function() {
  had <- !is.null(the$scope)
  the$scope <- NULL
  invisible(had)
}

# return the active scope or fail with a clear message
#' @noRd
assert_weasel_scope <- function() {
  if (is.null(the$scope)) {
    .weasel_stop("no scope set; run set_weasel_scope() first.",
                 class = "weasel_error_no_scope")
  }
  the$scope
}

#' Inspect the active weasel scope
#'
#' Prints a compact status report of the active scope: the bound data,
#' the grid and span, the structural constraints, and how far the
#' pipeline has progressed. Prints "no active weasel scope." when no
#' scope is set.
#'
#' @return Invisibly, a list with the scope settings (`id`, `wave`,
#'   `grid`, `lower`, `upper`, `span`, `size`, `gap`, `n_gap`) and the
#'   progress counters (`n_rows`, `n_kept`, `n_patterns`), or `NULL`
#'   when no scope is active.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 30, n_times = 6, seed = 1)
#' set_weasel_scope(d, "id", "time", gap = 1)
#' weasel_scope_info()
#' weasel_reshape_to_wide()
#' weasel_summarize_waves()
#' weasel_scope_info()
#' weasel_clear_scope()
#'
#' @export
weasel_scope_info <- function() {
  if (is.null(the$scope)) {
    cat("no active weasel scope.\n")
    return(invisible(NULL))
  }
  env <- the$scope

  cat("weasel scope\n")
  cat("  data:        ", nrow(env$data), " rows; id = '", env$id,
      "', wave = '", env$wave, "'\n", sep = "")
  cat("  grid:        ", env$grid, "\n", sep = "")
  if (!is.null(env$span)) {
    cat("  span:        ", env$lower, ":", env$upper,
        " (", length(env$span), " waves)\n", sep = "")
  } else {
    req <- if (!is.null(env$lower) || !is.null(env$upper)) {
      paste0(" (requested ", .weasel_or(env$lower, "min"), ":",
             .weasel_or(env$upper, "max"), ")")
    } else {
      ""
    }
    cat("  span:        not evaluated yet", req, "\n", sep = "")
  }
  cons <- character(0)
  if (!is.null(env$size))  cons <- c(cons, paste0("size >= ", min(env$size)))
  if (!is.null(env$gap))   cons <- c(cons, paste0("max interior gap <= ", env$gap))
  if (!is.null(env$n_gap)) cons <- c(cons, paste0("interior gaps <= ", env$n_gap))
  cat("  constraints: ",
      if (length(cons) > 0) paste(cons, collapse = ", ") else "none",
      "\n", sep = "")
  stage <- c(
    "set",
    if (!is.null(env$span)) "evaluated",
    if (!is.null(env$pivot)) paste0("reshaped (", nrow(env$pivot), " kept)"),
    if (!is.null(env$view)) paste0("summarized (", nrow(env$view), " patterns)")
  )
  cat("  stage:       ", paste(stage, collapse = " -> "), "\n", sep = "")

  invisible(list(
    id = env$id, wave = env$wave, grid = env$grid,
    lower = env$lower, upper = env$upper, span = env$span,
    size = env$size, gap = env$gap, n_gap = env$n_gap,
    n_rows = nrow(env$data),
    n_kept = if (!is.null(env$pivot)) nrow(env$pivot) else NA_integer_,
    n_patterns = if (!is.null(env$view)) nrow(env$view) else NA_integer_
  ))
}

#' Evaluate wave bounds, grid, and valid window sizes
#'
#' Finalises `lower`, `upper`, the wave grid, and `size` within the
#' active scope. Called automatically by [weasel_reshape_to_wide()], but
#' can be run explicitly to inspect bounds before reshaping. With
#' `grid = "consecutive"`, a warning (class `weasel_empty_waves`) is
#' issued once per scope when the span contains waves that no respondent
#' has, since such waves count as missed by everyone.
#'
#' @return The scope environment, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' weasel_clear_scope()
#'
#' @export
evaluate_weasel_scope <- function() {
  env <- assert_weasel_scope()

  waves <- .weasel_check_wave(env$data[[env$wave]], env$wave)

  if (is.null(env$lower)) env$lower <- min(waves)
  if (is.null(env$upper)) env$upper <- max(waves)
  env$lower <- as.integer(env$lower)
  env$upper <- as.integer(env$upper)
  if (env$upper < env$lower) .weasel_stop("upper must be >= lower.")

  env$observed_waves <- waves
  env$span <- .weasel_grid_waves(waves, env$lower, env$upper, env$grid)
  if (env$grid == "consecutive" && !isTRUE(env$warned_empty)) {
    .weasel_warn_empty_span(env$span, waves)
    env$warned_empty <- TRUE
  }

  span_len <- length(env$span)
  if (is.null(env$size)) {
    env$size <- seq.int(min(3L, span_len), span_len)
  }
  env$span_len   <- span_len
  env$valid_size <- env$size[env$size <= span_len]
  if (length(env$valid_size) == 0) {
    .weasel_stop("no valid window size: the span ", env$lower, ":", env$upper,
                 " has length ", span_len,
                 " but the requested size values are ",
                 paste(env$size, collapse = ", "), ".")
  }
  env$min_obs <- min(env$valid_size)

  invisible(env)
}

#' Reshape scoped data to a wide presence matrix
#'
#' Builds a respondent x wave data frame where each cell contains the
#' wave number if the respondent is observed, and `NA` otherwise.
#' Respondents with fewer than `min(valid size)` observed waves are
#' dropped, and the optional `gap`/`n_gap` constraints from
#' [set_weasel_scope()] are applied to interior gaps. Duplicated
#' (id, wave) rows are counted once and trigger a warning.
#'
#' @return The pivot data frame, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' weasel_reshape_to_wide()
#' weasel_clear_scope()
#'
#' @export
weasel_reshape_to_wide <- function() {
  env <- assert_weasel_scope()
  evaluate_weasel_scope()

  dat      <- env$data
  id_col   <- env$id
  wave_col <- env$wave
  span     <- env$span
  L        <- length(span)

  ok    <- !is.na(dat[[id_col]]) & !is.na(dat[[wave_col]])
  ids0  <- dat[[id_col]][ok]
  w0    <- as.integer(round(dat[[wave_col]][ok]))
  in_sp <- w0 %in% span
  ids_v <- ids0[in_sp]
  w_v   <- w0[in_sp]
  if (length(ids_v) == 0) .weasel_stop("no rows in the selected span.")

  dd <- .weasel_dedup_index(ids_v, w_v)
  .weasel_warn_duplicates(dd$n_dup, id_col, wave_col)
  ids_v <- ids_v[dd$idx]
  w_v   <- w_v[dd$idx]

  pos <- match(w_v, span)
  met <- .weasel_gap_metrics(ids_v, pos, L)

  keep <- met$n_present >= env$min_obs
  if (!is.null(env$gap)) {
    keep <- keep & met$max_gap <= env$gap
  }
  if (!is.null(env$n_gap)) {
    keep <- keep & met$n_gap <= env$n_gap
  }

  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    .weasel_msg(n_dropped, " respondent(s) dropped by size/gap constraints; ",
                sum(keep), " kept.")
  }

  ids2 <- met$id[keep]
  if (length(ids2) == 0) {
    .weasel_stop("no respondents satisfy the size/gap constraints.")
  }

  sel <- ids_v %in% ids2
  m <- matrix(NA_integer_, nrow = length(ids2), ncol = L)
  rownames(m) <- as.character(ids2)
  colnames(m) <- as.character(span)
  i <- match(ids_v[sel], ids2)
  j <- pos[sel]
  m[cbind(i, j)] <- span[j]

  pivot <- data.frame(id_value = ids2, stringsAsFactors = FALSE)
  names(pivot)[1] <- id_col
  for (k in seq_len(ncol(m))) {
    pivot[[colnames(m)[k]]] <- m[, k]
  }

  env$pivot         <- pivot
  env$L             <- L
  env$scope_metrics <- met

  invisible(pivot)
}

# ---- wave pattern summary ------------------------------------------------

# shape-safe pattern summary; works for any number of rows including one
#' @noRd
.summarize_patterns <- function(pivot, id_col, span_cols) {
  mat <- as.matrix(pivot[, span_cols, drop = FALSE])
  if (nrow(mat) == 0) {
    return(list(
      view = data.frame(waves = character(0), n = integer(0),
                        ids = integer(0), stringsAsFactors = FALSE),
      waves_by_id = character(0)
    ))
  }

  mat_chr <- matrix(ifelse(is.na(mat), ".", as.character(mat)),
                    nrow = nrow(mat))
  pat <- do.call(paste, c(as.data.frame(mat_chr, stringsAsFactors = FALSE),
                          list(sep = " ")))
  present_n <- as.integer(rowSums(!is.na(mat)))

  tab      <- table(pat)
  patterns <- names(tab)

  view <- data.frame(
    waves = patterns,
    n     = present_n[match(patterns, pat)],
    ids   = as.integer(tab),
    stringsAsFactors = FALSE
  )
  view <- view[order(-view$ids, -view$n), , drop = FALSE]
  rownames(view) <- NULL

  list(view = view, waves_by_id = pat)
}

#' Summarize wave patterns in the scoped data
#'
#' Groups respondents by their observed-wave pattern and counts how many
#' share each pattern. Requires [weasel_reshape_to_wide()] to have been
#' called. In the result, `waves` is the pattern (a `.` marks a missing
#' wave), `n` is the number of observed waves in that pattern, and `ids`
#' is the number of respondents sharing it. Rows are sorted by `ids`,
#' then `n`, both descending.
#'
#' @return The summary data frame, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' weasel_reshape_to_wide()
#' weasel_summarize_waves()
#' weasel_clear_scope()
#'
#' @export
weasel_summarize_waves <- function() {
  env <- assert_weasel_scope()
  if (is.null(env$pivot)) .weasel_stop("run weasel_reshape_to_wide() first.")

  span_cols <- setdiff(names(env$pivot), env$id)
  res <- .summarize_patterns(env$pivot, env$id, span_cols)

  env$view        <- res$view
  env$waves_by_id <- res$waves_by_id

  invisible(env$view)
}

#' Filter the wave-pattern summary
#'
#' Narrows the pattern table produced by [weasel_summarize_waves()].
#'
#' @param n_range Optional length-2 numeric vector giving the min/max
#'   number of observed waves per pattern (the `n` column).
#' @param ids_range Optional length-2 numeric vector giving the min/max
#'   respondent count per pattern (the `ids` column).
#'
#' @return A filtered data frame.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' weasel_reshape_to_wide()
#' weasel_summarize_waves()
#'
#' # only patterns with at least 6 observed waves
#' weasel_print_table(weasel_filter_wave_summary(n_range = c(6, Inf)))
#'
#' # only patterns shared by at least 3 respondents
#' weasel_print_table(weasel_filter_wave_summary(ids_range = c(3, Inf)))
#' weasel_clear_scope()
#'
#' @export
weasel_filter_wave_summary <- function(n_range = NULL, ids_range = NULL) {
  env <- assert_weasel_scope()
  if (is.null(env$view)) .weasel_stop("run weasel_summarize_waves() first.")
  if (!is.null(n_range) && length(n_range) != 2) {
    .weasel_stop("n_range must be a length-2 numeric vector.")
  }
  if (!is.null(ids_range) && length(ids_range) != 2) {
    .weasel_stop("ids_range must be a length-2 numeric vector.")
  }

  v <- env$view
  if (!is.null(n_range)) {
    v <- v[v$n >= min(n_range) & v$n <= max(n_range), , drop = FALSE]
  }
  if (!is.null(ids_range)) {
    v <- v[v$ids >= min(ids_range) & v$ids <= max(ids_range), , drop = FALSE]
  }
  v
}

#' Retrieve long-format data matching wave-pattern rows
#'
#' After [weasel_summarize_waves()] has grouped respondents into
#' patterns, this function extracts the long-format rows for every
#' respondent who matches the given pattern row(s). Use
#' [weasel_filter_wave_summary()] first to identify which row indices to
#' request.
#'
#' @param i Integer vector of row indices into the wave-pattern summary;
#'   respondents matching any of the selected patterns are returned.
#'   Defaults to the first row.
#' @param within_span If `TRUE`, only rows whose wave lies inside the
#'   scoped span are returned. If `FALSE` (default), all rows of the
#'   matching respondents are returned, including waves outside the
#'   span.
#'
#' @return A data frame (subset of the original long data).
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' weasel_reshape_to_wide()
#' weasel_summarize_waves()
#'
#' subset1 <- weasel_get_data_by_row(1)
#' weasel_print_table(head(subset1), title = "Row-1 data preview")
#'
#' # respondents matching either of the two most common patterns
#' subset12 <- weasel_get_data_by_row(c(1, 2))
#' weasel_clear_scope()
#'
#' @export
weasel_get_data_by_row <- function(i = 1L, within_span = FALSE) {
  env <- assert_weasel_scope()
  if (is.null(env$view) || is.null(env$waves_by_id) || is.null(env$pivot)) {
    .weasel_stop("run weasel_reshape_to_wide() and weasel_summarize_waves() first.")
  }
  if (nrow(env$view) == 0) .weasel_stop("the pattern summary is empty.")

  i <- suppressWarnings(as.integer(i))
  if (length(i) == 0 || anyNA(i) || any(i < 1) || any(i > nrow(env$view))) {
    .weasel_stop("row index out of range.")
  }
  i <- unique(i)

  target   <- env$view$waves[i]
  id_col   <- env$id
  wave_col <- env$wave
  dat      <- env$data

  keep_ids <- env$pivot[[id_col]][env$waves_by_id %in% target]
  out <- dat[dat[[id_col]] %in% keep_ids, , drop = FALSE]
  if (isTRUE(within_span)) {
    w_int <- as.integer(round(out[[wave_col]]))
    out <- out[w_int %in% env$span, , drop = FALSE]
  }
  out
}

# ---- deprecated names ------------------------------------------------------

#' @rdname weasel-deprecated
#' @export
reshape_to_wide <- function(...) {
  .weasel_deprecate("reshape_to_wide", "weasel_reshape_to_wide")
  weasel_reshape_to_wide(...)
}

#' @rdname weasel-deprecated
#' @export
summarize_waves <- function(...) {
  .weasel_deprecate("summarize_waves", "weasel_summarize_waves")
  weasel_summarize_waves(...)
}

#' @rdname weasel-deprecated
#' @export
filter_wave_summary <- function(...) {
  .weasel_deprecate("filter_wave_summary", "weasel_filter_wave_summary")
  weasel_filter_wave_summary(...)
}

#' @rdname weasel-deprecated
#' @export
get_data_by_row <- function(...) {
  .weasel_deprecate("get_data_by_row", "weasel_get_data_by_row")
  weasel_get_data_by_row(...)
}

#' @rdname weasel-deprecated
#' @export
generate_sets <- function() {
  .weasel_deprecate("generate_sets", "evaluate_weasel_scope")
  env <- assert_weasel_scope()
  evaluate_weasel_scope()
  invisible(env)
}

#' @rdname weasel-deprecated
#' @export
filter_sets <- function() {
  .weasel_deprecate("filter_sets", "weasel_filter_wave_summary")
  invisible(assert_weasel_scope())
}
