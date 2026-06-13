# scope-based interactive pipeline
# state lives in the package-internal environment `the` (see 01-utils.R),
# never in the user's global environment

#' Set up a weasel scope for wave-pattern analysis
#'
#' Initialises the package-internal scope that holds data, column names,
#' and wave-range parameters used by the downstream pipeline functions.
#' The user's global environment is never modified.
#'
#' @param data A data frame in long format. A respondent is considered
#'   observed at a wave if a row with that (id, wave) pair exists.
#' @param id Name of the respondent-identifier column. Any atomic type
#'   (integer, character, ...) is supported.
#' @param wave Name of the wave/time column. Must be numeric with
#'   integer-valued entries.
#' @param size Optional integer vector of acceptable per-respondent
#'   observation counts; respondents with fewer than `min(size)`
#'   observed waves are dropped by [reshape_to_wide()]. Defaults to
#'   `min(3, span length)` through the span length.
#' @param lower Optional lower bound of the wave range.
#' @param upper Optional upper bound of the wave range.
#' @param gap Optional integer; maximum allowed length of an interior
#'   gap (a run of missing waves strictly between a respondent's first
#'   and last observed wave inside the span). `NULL` (default) applies
#'   no constraint.
#' @param n_gap Optional integer; maximum allowed number of interior
#'   gaps. `NULL` (default) applies no constraint.
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
                             override = TRUE) {
  .weasel_check_id_wave(data, id, wave)

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
  env$gap   <- if (is.null(gap)) NULL else as.integer(gap)
  env$n_gap <- if (is.null(n_gap)) NULL else as.integer(n_gap)
  env$row   <- 1L

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
    .weasel_stop("no scope set; run set_weasel_scope() first.")
  }
  the$scope
}

#' Evaluate wave bounds and valid window sizes
#'
#' Finalises `lower`, `upper`, and `size` within the active scope.
#' Called automatically by [reshape_to_wide()], but can be run
#' explicitly to inspect bounds before reshaping.
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

  span_len <- length(.weasel_seq_int(env$lower, env$upper))
  if (is.null(env$size) || length(env$size) == 0) {
    env$size <- seq.int(min(3L, span_len), span_len)
  } else {
    env$size <- as.integer(env$size)
    env$size <- env$size[!is.na(env$size) & env$size >= 1L]
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
#' Builds a respondent x wave matrix where each cell contains the wave
#' number if the respondent is observed, and `NA` otherwise. Respondents
#' with fewer than `min(valid size)` observed waves are dropped, and the
#' optional `gap`/`n_gap` constraints from [set_weasel_scope()] are
#' applied to interior gaps.
#'
#' @return The pivot data frame, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' reshape_to_wide()
#' weasel_clear_scope()
#'
#' @export
reshape_to_wide <- function() {
  env <- assert_weasel_scope()
  evaluate_weasel_scope()

  dat      <- env$data
  id_col   <- env$id
  wave_col <- env$wave
  lower    <- env$lower
  upper    <- env$upper
  span     <- .weasel_seq_int(lower, upper)
  L        <- length(span)

  d <- dat[!is.na(dat[[id_col]]) & !is.na(dat[[wave_col]]),
           c(id_col, wave_col), drop = FALSE]
  d <- d[d[[wave_col]] >= lower & d[[wave_col]] <= upper, , drop = FALSE]
  d <- unique(d)

  ids <- sort(unique(d[[id_col]]))
  if (length(ids) == 0) .weasel_stop("no rows in the selected span.")

  m <- matrix(NA_integer_, nrow = length(ids), ncol = L)
  rownames(m) <- as.character(ids)
  colnames(m) <- as.character(span)

  i  <- match(d[[id_col]], ids)
  j  <- match(as.integer(d[[wave_col]]), span)
  ok <- !is.na(i) & !is.na(j)
  m[cbind(i[ok], j[ok])] <- span[j[ok]]

  present <- !is.na(m)
  keep <- rowSums(present) >= env$min_obs

  if (!is.null(env$gap) || !is.null(env$n_gap)) {
    gaps <- apply(present, 1, .weasel_interior_gaps)
    if (!is.null(env$gap)) {
      keep <- keep & vapply(gaps, function(g) g$max_gap <= env$gap, logical(1))
    }
    if (!is.null(env$n_gap)) {
      keep <- keep & vapply(gaps, function(g) g$n_gap <= env$n_gap, logical(1))
    }
  }

  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    .weasel_msg(n_dropped, " respondent(s) dropped by size/gap constraints; ",
                sum(keep), " kept.")
  }

  m2   <- m[keep, , drop = FALSE]
  ids2 <- ids[keep]
  if (length(ids2) == 0) {
    .weasel_stop("no respondents satisfy the size/gap constraints.")
  }

  pivot <- data.frame(id_value = ids2, stringsAsFactors = FALSE)
  names(pivot)[1] <- id_col
  for (k in seq_len(ncol(m2))) {
    pivot[[colnames(m2)[k]]] <- m2[, k]
  }

  env$pivot <- pivot
  env$L     <- L

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
#' share each pattern. Requires [reshape_to_wide()] to have been called.
#' In the result, `waves` is the pattern (a `.` marks a missing wave),
#' `n` is the number of observed waves in that pattern, and `ids` is the
#' number of respondents sharing it. Rows are sorted by `ids`, then `n`,
#' both descending.
#'
#' @return The summary data frame, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' reshape_to_wide()
#' summarize_waves()
#' weasel_clear_scope()
#'
#' @export
summarize_waves <- function() {
  env <- assert_weasel_scope()
  if (is.null(env$pivot)) .weasel_stop("run reshape_to_wide() first.")

  span_cols <- setdiff(names(env$pivot), env$id)
  res <- .summarize_patterns(env$pivot, env$id, span_cols)

  env$view        <- res$view
  env$waves_by_id <- res$waves_by_id

  invisible(env$view)
}

#' Filter the wave-pattern summary
#'
#' Narrows the pattern table produced by [summarize_waves()].
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
#' reshape_to_wide()
#' summarize_waves()
#'
#' # only patterns with at least 6 observed waves
#' weasel_print_table(filter_wave_summary(n_range = c(6, Inf)))
#'
#' # only patterns shared by at least 3 respondents
#' weasel_print_table(filter_wave_summary(ids_range = c(3, Inf)))
#' weasel_clear_scope()
#'
#' @export
filter_wave_summary <- function(n_range = NULL, ids_range = NULL) {
  env <- assert_weasel_scope()
  if (is.null(env$view)) .weasel_stop("run summarize_waves() first.")

  v <- env$view
  if (!is.null(n_range) && length(n_range) == 2) {
    v <- v[v$n >= min(n_range) & v$n <= max(n_range), , drop = FALSE]
  }
  if (!is.null(ids_range) && length(ids_range) == 2) {
    v <- v[v$ids >= min(ids_range) & v$ids <= max(ids_range), , drop = FALSE]
  }
  v
}

#' Retrieve long-format data matching a wave-pattern row
#'
#' After [summarize_waves()] has grouped respondents into patterns,
#' this function extracts the long-format rows for every respondent who
#' matches a given pattern. Use [filter_wave_summary()] first to
#' identify which row index to request.
#'
#' @param i Row index into the wave-pattern summary. Defaults to the
#'   last-used index.
#' @param within_span If `TRUE`, only rows whose wave lies inside the
#'   scoped `lower:upper` span are returned. If `FALSE` (default), all
#'   rows of the matching respondents are returned, including waves
#'   outside the span.
#'
#' @return A data frame (subset of the original long data).
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' reshape_to_wide()
#' summarize_waves()
#'
#' subset1 <- get_data_by_row(1)
#' weasel_print_table(head(subset1), title = "Row-1 data preview")
#' weasel_clear_scope()
#'
#' @export
get_data_by_row <- function(i = NULL, within_span = FALSE) {
  env <- assert_weasel_scope()
  if (is.null(env$view) || is.null(env$waves_by_id) || is.null(env$pivot)) {
    .weasel_stop("run reshape_to_wide() and summarize_waves() first.")
  }
  if (nrow(env$view) == 0) .weasel_stop("the pattern summary is empty.")

  if (is.null(i)) i <- env$row
  i <- as.integer(i)
  if (is.na(i) || i < 1 || i > nrow(env$view)) {
    .weasel_stop("row index out of range.")
  }
  env$row <- i

  target   <- env$view$waves[i]
  id_col   <- env$id
  wave_col <- env$wave
  dat      <- env$data

  keep_ids <- env$pivot[[id_col]][env$waves_by_id == target]
  out <- dat[dat[[id_col]] %in% keep_ids, , drop = FALSE]
  if (isTRUE(within_span)) {
    out <- out[!is.na(out[[wave_col]]) &
                 out[[wave_col]] >= env$lower &
                 out[[wave_col]] <= env$upper, , drop = FALSE]
  }
  out
}

# ---- legacy stubs ---------------------------------------------------------

#' Generate sets (deprecated no-op)
#'
#' Retained for backward compatibility with older scripts that called
#' `generate_sets()` between [evaluate_weasel_scope()] and
#' [reshape_to_wide()]. It only re-runs [evaluate_weasel_scope()];
#' omit it in new code.
#'
#' @return The scope environment, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 30, n_times = 6, seed = 1)
#' set_weasel_scope(d, "id", "time")
#' evaluate_weasel_scope()
#' generate_sets()
#' filter_sets()
#' weasel_clear_scope()
#'
#' @export
generate_sets <- function() {
  env <- assert_weasel_scope()
  evaluate_weasel_scope()
  invisible(env)
}

#' Filter sets (deprecated no-op)
#'
#' Retained for backward compatibility. Does nothing; omit it in new
#' code.
#'
#' @return The scope environment, invisibly.
#'
#' @export
filter_sets <- function() {
  invisible(assert_weasel_scope())
}
