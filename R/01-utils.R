# internal utility helpers and package state
# none of these are exported; helpers are prefixed with .weasel_

# package-internal state container (replaces the former global weasel_env)
the <- new.env(parent = emptyenv())

# classed error so callers can handle weasel failures programmatically,
# e.g. tryCatch(..., weasel_error = function(e) ...)
.weasel_stop <- function(..., class = NULL) {
  stop(errorCondition(paste0(...), class = c(class, "weasel_error"),
                      call = NULL))
}

# classed warning; always a real warning condition so behaviour does not
# depend on which optional packages are installed
.weasel_warn <- function(..., class = NULL) {
  warning(warningCondition(paste0(...), class = c(class, "weasel_warning"),
                           call = NULL))
}

# deprecation warning used by the renamed and legacy functions
.weasel_deprecate <- function(old, new) {
  .weasel_warn(old, "() is deprecated; use ", new, "() instead.",
               class = "weasel_deprecated")
}

.weasel_is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.weasel_cli <- function() .weasel_is_installed("cli")

.weasel_verbose <- function() isTRUE(getOption("weasel.verbose", TRUE))

.weasel_msg <- function(...) {
  if (!.weasel_verbose()) return(invisible(NULL))
  text <- paste0(...)
  if (.weasel_cli()) cli::cli_inform(text) else message(text)
  invisible(NULL)
}

.weasel_ok <- function(...) {
  if (!.weasel_verbose()) return(invisible(NULL))
  text <- paste0(...)
  if (.weasel_cli()) cli::cli_alert_success(text) else message(paste0("ok: ", text))
  invisible(NULL)
}

.weasel_h2 <- function(text) {
  if (!.weasel_verbose()) return(invisible(NULL))
  if (.weasel_cli()) {
    cli::cli_rule(left = text)
  } else {
    w <- max(getOption("width", 72L), nchar(text) + 6L)
    dashes <- paste(rep("-", max(w - nchar(text) - 3L, 3L)), collapse = "")
    message(paste0("- ", text, " ", dashes))
  }
  invisible(NULL)
}

# null-default helper
.weasel_or <- function(a, b) if (is.null(a)) b else a

# integer sequence that never counts downwards: a > b yields integer(0)
.weasel_seq_int <- function(a, b) {
  a <- as.integer(a)
  b <- as.integer(b)
  if (is.na(a) || is.na(b) || a > b) integer(0) else seq.int(a, b)
}

.weasel_unique_int <- function(x) sort(unique(as.integer(x[!is.na(x)])))

# single non-negative integer or NULL; strict: fractional values are
# rejected rather than silently truncated
.weasel_check_count <- function(x, name) {
  if (is.null(x)) return(NULL)
  ok <- length(x) == 1 && is.numeric(x) && !is.na(x) && is.finite(x) &&
    abs(x - round(x)) <= 1e-8 && x >= 0
  if (!ok) {
    .weasel_stop(name, " must be a single non-negative integer ",
                 "(fractional values are rejected, not truncated).")
  }
  as.integer(round(x))
}

# single integer bound (any sign) or NULL; strict: fractional values
# are rejected rather than silently rounded
.weasel_check_bound <- function(x, name) {
  if (is.null(x)) return(NULL)
  ok <- length(x) == 1 && is.numeric(x) && !is.na(x) && is.finite(x) &&
    abs(x - round(x)) <= 1e-8
  if (!ok) {
    .weasel_stop(name, " must be a single integer-valued number ",
                 "(fractional values are rejected, not rounded).")
  }
  as.integer(round(x))
}

# validate a wave column: must be numeric and integer-valued
# returns the sorted unique integer waves
.weasel_check_wave <- function(x, name = "wave") {
  if (is.factor(x)) {
    .weasel_stop(
      "column '", name, "' is a factor; convert it first, e.g. ",
      "as.integer(as.character(data$", name, "))."
    )
  }
  if (!is.numeric(x)) {
    .weasel_stop("column '", name, "' must be numeric (integer wave numbers).")
  }
  ok <- x[!is.na(x)]
  if (length(ok) == 0) .weasel_stop("column '", name, "' has no non-missing values.")
  if (any(abs(ok - round(ok)) > 1e-8)) {
    .weasel_stop("column '", name, "' must contain integer-valued wave numbers.")
  }
  sort(unique(as.integer(round(ok))))
}

# shared entry validation for both pipelines
.weasel_check_id_wave <- function(data, id, wave) {
  if (!is.data.frame(data)) .weasel_stop("data must be a data.frame.")
  if (!is.character(id) || length(id) != 1) .weasel_stop("id must be a single string.")
  if (!is.character(wave) || length(wave) != 1) .weasel_stop("wave must be a single string.")
  if (identical(id, wave)) {
    .weasel_stop("id and wave must be different columns (both are '",
                 id, "').")
  }
  if (!(id %in% names(data))) .weasel_stop("id column not found: ", id)
  if (!(wave %in% names(data))) .weasel_stop("wave column not found: ", wave)
  if (is.list(data[[id]])) .weasel_stop("id column must be an atomic vector.")
  invisible(.weasel_check_wave(data[[wave]], wave))
}

# one-line account of rows excluded from participation analysis, so no
# observation disappears silently; verbose-gated like all status text
.weasel_report_dropped <- function(data, id, wave, span) {
  na_id   <- sum(is.na(data[[id]]))
  na_wave <- sum(!is.na(data[[id]]) & is.na(data[[wave]]))
  w <- suppressWarnings(as.integer(round(as.numeric(data[[wave]]))))
  out_span <- sum(!is.na(data[[id]]) & !is.na(w) & !(w %in% span))
  if (na_id + na_wave + out_span > 0) {
    .weasel_msg(
      "rows not used for participation: ", na_id, " with missing ", id,
      ", ", na_wave, " with missing ", wave, ", ", out_span,
      " outside span ", span[1], ":", span[length(span)], "."
    )
  }
  invisible(c(na_id = na_id, na_wave = na_wave, out_of_span = out_span))
}

# warn when a returned long-format subset still contains duplicated
# (id, wave) rows: participation metrics count each pair once, but
# output rows are returned as-is
.weasel_warn_output_duplicates <- function(out, id, wave) {
  if (nrow(out) == 0) return(invisible(0L))
  dd <- .weasel_dedup_index(out[[id]],
                            as.integer(round(as.numeric(out[[wave]]))))
  if (dd$n_dup > 0) {
    .weasel_warn(
      dd$n_dup, " duplicated (", id, ", ", wave, ") row(s) in the ",
      "returned data. selection metrics counted each pair once, but ",
      "output rows are not deduplicated; resolve duplicates before ",
      "modelling, e.g. df[!duplicated(df[c(\"", id, "\", \"", wave,
      "\")]), ].",
      class = "weasel_duplicates"
    )
  }
  invisible(dd$n_dup)
}

# sorted index of the unique (id, wave) pairs plus the duplicate count;
# a single order() pass, much faster on large panels than the
# paste-based unique()/duplicated() methods for data frames
.weasel_dedup_index <- function(id_vec, wave_vec) {
  o <- order(id_vec, wave_vec)
  n <- length(o)
  if (n <= 1L) return(list(idx = o, n_dup = 0L))
  a <- id_vec[o]
  b <- wave_vec[o]
  dup <- c(FALSE, a[-1L] == a[-n] & b[-1L] == b[-n])
  list(idx = o[!dup], n_dup = as.integer(sum(dup, na.rm = TRUE)))
}

# warn about duplicated (id, wave) rows; duplicates usually indicate a
# join or merge problem, and both pipelines count each pair once
.weasel_warn_duplicates <- function(n_dup, id, wave) {
  if (n_dup > 0) {
    .weasel_warn(
      n_dup, " duplicated (", id, ", ", wave, ") row(s) found; each pair ",
      "is counted once. duplicates often indicate a join or merge ",
      "problem worth checking.",
      class = "weasel_duplicates"
    )
  }
  invisible(n_dup)
}

.weasel_check_duplicates <- function(data, id, wave) {
  dd <- .weasel_dedup_index(data[[id]], data[[wave]])
  .weasel_warn_duplicates(dd$n_dup, id, wave)
}

# resolve the wave grid over which presence is evaluated:
# "consecutive" treats every integer in lower:upper as a scheduled wave;
# "observed" uses only wave values that occur anywhere in the data,
# which suits biennial and other non-consecutive schedules
.weasel_grid_waves <- function(observed_waves, lower, upper, grid) {
  span <- if (grid == "observed") {
    observed_waves[observed_waves >= lower & observed_waves <= upper]
  } else {
    .weasel_seq_int(lower, upper)
  }
  if (length(span) == 0) .weasel_stop("the selected span contains no waves.")
  span
}

# warn when a consecutive span contains waves that no respondent has:
# every such wave counts as missed by everyone, which usually means the
# schedule is non-consecutive and grid = "observed" is the right choice
.weasel_warn_empty_span <- function(span, observed_waves) {
  empty <- setdiff(span, observed_waves)
  if (length(empty) == 0) return(invisible(0L))
  shown <- paste(utils::head(empty, 5), collapse = ", ")
  if (length(empty) > 5) shown <- paste0(shown, ", ...")
  .weasel_warn(
    length(empty), " of ", length(span), " wave(s) in the span ",
    span[1], ":", span[length(span)],
    " have no observations for any respondent (", shown, "); they count ",
    "as missed by everyone. if waves are non-consecutive by design ",
    "(e.g. biennial), use grid = \"observed\".",
    class = "weasel_empty_waves"
  )
  invisible(length(empty))
}

# validate a custom scenario table for weasel_plan()
.weasel_check_scenarios <- function(scenarios) {
  required <- c("scenario", "require_endpoints", "max_missing",
                "n_gap_max", "max_gap_max")
  if (!is.data.frame(scenarios)) .weasel_stop("scenarios must be a data.frame.")
  missing_cols <- setdiff(required, names(scenarios))
  if (length(missing_cols) > 0) {
    .weasel_stop("scenarios is missing required column(s): ",
                 paste(missing_cols, collapse = ", "))
  }
  if (nrow(scenarios) == 0) .weasel_stop("scenarios must have at least one row.")
  s <- scenarios[required]
  s$scenario <- as.character(s$scenario)
  if (anyNA(s$scenario) || any(!nzchar(s$scenario))) {
    .weasel_stop("scenario names must be non-empty strings.")
  }
  if (anyDuplicated(s$scenario)) .weasel_stop("scenario names must be unique.")
  s$require_endpoints <- as.logical(s$require_endpoints)
  if (anyNA(s$require_endpoints)) {
    .weasel_stop("scenario column 'require_endpoints' must not contain NA.")
  }
  for (nm in c("max_missing", "n_gap_max", "max_gap_max")) {
    s[[nm]] <- suppressWarnings(as.numeric(s[[nm]]))
    if (anyNA(s[[nm]])) {
      .weasel_stop("scenario column '", nm, "' must not contain NA.")
    }
    if (any(s[[nm]] < 0)) .weasel_stop("scenario column '", nm, "' must be >= 0.")
    frac <- is.finite(s[[nm]]) & abs(s[[nm]] - round(s[[nm]])) > 1e-8
    if (any(frac)) {
      .weasel_stop("scenario column '", nm, "' must contain integer-valued ",
                   "tolerances or Inf (fractional values are rejected, ",
                   "not truncated).")
    }
  }
  s
}

# run-length gap metrics over the full presence vector
# a gap is any maximal run of FALSE, including leading/trailing runs
# (reference implementation; the pipelines use .weasel_gap_metrics)
.weasel_rle_gaps <- function(present_logical) {
  if (length(present_logical) == 0) {
    return(list(n_gap = 0L, max_gap = 0L, n_present = 0L))
  }
  miss <- !present_logical
  r <- rle(miss)
  n_gap <- sum(r$values)
  max_gap <- if (any(r$values)) max(r$lengths[r$values]) else 0L
  list(
    n_gap     = as.integer(n_gap),
    max_gap   = as.integer(max_gap),
    n_present = as.integer(sum(present_logical))
  )
}

# interior gap metrics: runs of FALSE strictly between the first and last
# TRUE; leading/trailing absence is entry/exit, not a gap
# (reference implementation; the pipelines use .weasel_gap_metrics)
.weasel_interior_gaps <- function(present_logical) {
  n_present <- as.integer(sum(present_logical))
  if (n_present == 0L) {
    return(list(n_gap = 0L, max_gap = 0L, n_present = 0L))
  }
  idx <- which(present_logical)
  core <- present_logical[idx[1]:idx[length(idx)]]
  g <- .weasel_rle_gaps(core)
  list(n_gap = g$n_gap, max_gap = g$max_gap, n_present = n_present)
}

# vectorized per-respondent presence metrics from long-format positions;
# equivalent to applying .weasel_interior_gaps row by row, but a single
# ordered pass over the observations instead of one rle() per respondent
# id_vec:  respondent id per deduplicated observation (any atomic type)
# pos_vec: integer grid position in 1..L
# returns one row per id, ids sorted ascending
.weasel_gap_metrics <- function(id_vec, pos_vec, L) {
  if (length(id_vec) == 0) {
    return(data.frame(
      id = id_vec, n_present = integer(0),
      has_lower = logical(0), has_upper = logical(0),
      n_gap = integer(0), max_gap = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  o     <- order(id_vec, pos_vec)
  ids_o <- id_vec[o]
  pos_o <- pos_vec[o]

  new_id <- c(TRUE, ids_o[-1L] != ids_o[-length(ids_o)])
  grp    <- cumsum(new_id)
  n_ids  <- grp[length(grp)]

  n_present <- tabulate(grp, nbins = n_ids)
  has_lower <- tabulate(grp[pos_o == 1L], nbins = n_ids) > 0L
  has_upper <- tabulate(grp[pos_o == L],  nbins = n_ids) > 0L

  n_gap   <- integer(n_ids)
  max_gap <- integer(n_ids)
  if (length(pos_o) > 1L) {
    gap_len <- diff(pos_o) - 1L
    gap_len[new_id[-1L]] <- 0L
    idx <- which(gap_len > 0L)
    if (length(idx) > 0L) {
      g_grp <- grp[-1L][idx]
      g_val <- gap_len[idx]
      n_gap <- tabulate(g_grp, nbins = n_ids)
      o2    <- order(g_grp, g_val)
      last  <- !duplicated(g_grp[o2], fromLast = TRUE)
      max_gap[g_grp[o2][last]] <- g_val[o2][last]
    }
  }

  data.frame(
    id        = ids_o[new_id],
    n_present = as.integer(n_present),
    has_lower = has_lower,
    has_upper = has_upper,
    n_gap     = as.integer(n_gap),
    max_gap   = as.integer(max_gap),
    stringsAsFactors = FALSE
  )
}

# shared validation for plan objects
.weasel_check_plan <- function(plan_obj) {
  if (!is.list(plan_obj) || is.null(plan_obj$plan) ||
      !inherits(plan_obj$plan, "data.frame")) {
    .weasel_stop("plan_obj must be the output of weasel_plan().",
                 class = "weasel_error_plan")
  }
  if (!("scenario" %in% names(plan_obj$plan))) {
    .weasel_stop("plan_obj$plan must contain a 'scenario' column.",
                 class = "weasel_error_plan")
  }
  invisible(plan_obj)
}

# structural fingerprint of a panel, cheap and order-invariant; used to
# detect when a saved plan is reunited with data it was not built from
.weasel_data_fingerprint <- function(data, id, wave) {
  ok   <- !is.na(data[[id]]) & !is.na(data[[wave]])
  ids0 <- data[[id]][ok]
  w0   <- as.integer(round(as.numeric(data[[wave]][ok])))
  dd   <- .weasel_dedup_index(ids0, w0)
  idsu <- ids0[dd$idx]
  wu   <- w0[dd$idx]
  waves <- sort(unique(wu))
  list(
    n_rows         = nrow(data),
    n_pairs        = length(wu),
    n_ids          = length(unique(idsu)),
    id_type        = class(data[[id]])[1],
    waves          = waves,
    pairs_per_wave = as.integer(table(factor(wu, levels = waves)))
  )
}

# compare the stored fingerprint against explicitly supplied data and
# warn on a structural mismatch; plans from older versions carry no
# fingerprint and are accepted silently
.weasel_check_fingerprint <- function(plan_obj, data, id, wave) {
  fp <- plan_obj[["fingerprint"]]
  if (is.null(fp)) return(invisible(TRUE))
  now <- .weasel_data_fingerprint(data, id, wave)
  same <- identical(fp, now)
  if (!same) {
    .weasel_warn(
      "the supplied data do not structurally match the data this plan ",
      "was built from (rows ", fp$n_rows, " -> ", now$n_rows,
      ", unique (id, wave) pairs ", fp$n_pairs, " -> ", now$n_pairs,
      ", distinct ids ", fp$n_ids, " -> ", now$n_ids, "). the plan's ",
      "scenario id lists were computed on the original data; results ",
      "on different data may be invalid.",
      class = "weasel_data_mismatch"
    )
  }
  invisible(same)
}

# grid waves of a plan, with a fallback for objects saved by older
# versions that only stored the bounds; exact [[ ]] indexing is
# essential here: with $, a missing 'span' partial-matches
# 'span_reason' and the "full"/"core" label coerces to NA, silently
# emptying every downstream subset
.weasel_plan_span <- function(plan_obj, row) {
  sp <- plan_obj[["span"]]
  if (!is.null(sp)) return(as.integer(sp))
  .weasel_seq_int(row$lower[[1]], row$upper[[1]])
}

.weasel_format_num <- function(x, digits = 3) {
  if (length(x) == 0) return(character(0))
  out <- formatC(x, format = "f", digits = digits)
  out[is.na(x)] <- NA_character_
  out
}

.weasel_maybe_round <- function(x, digits = 3) {
  if (is.numeric(x)) round(x, digits) else x
}

# evaluate expr without leaving a net change in the caller's RNG state;
# saving/restoring .Random.seed in globalenv is the sanctioned mechanism
.weasel_with_preserved_seed <- function(expr) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  expr
}

#' Deprecated functions
#'
#' These earlier names are kept so existing scripts keep running, but they
#' now emit a deprecation warning (class `weasel_deprecated`) and forward
#' to their replacements. They will be removed in a future release.
#'
#' * `reshape_to_wide()` -> [weasel_reshape_to_wide()]
#' * `summarize_waves()` -> [weasel_summarize_waves()]
#' * `filter_wave_summary()` -> [weasel_filter_wave_summary()]
#' * `get_data_by_row()` -> [weasel_get_data_by_row()]
#' * `logo()` -> [weasel_logo()]
#' * `generate_sets()` and `filter_sets()` remain no-ops (see
#'   [evaluate_weasel_scope()]) and now warn as well.
#'
#' @param ... Arguments passed on to the replacement function.
#'
#' @return The value of the corresponding replacement function.
#' @name weasel-deprecated
#' @keywords internal
NULL
