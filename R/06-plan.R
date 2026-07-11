# scenario-based planning and comparison

# ---- internal helpers -----------------------------------------------------

# per-respondent presence metrics over the wave grid `span`;
# ids keep their original type; gaps are interior gaps (runs of missing
# grid waves strictly between a respondent's first and last observed wave)
#' @noRd
.weasel_id_metrics <- function(data, id_col, wave_col, span) {
  L <- length(span)

  ok    <- !is.na(data[[id_col]]) & !is.na(data[[wave_col]])
  ids0  <- data[[id_col]][ok]
  w0    <- as.integer(round(data[[wave_col]][ok]))
  in_sp <- w0 %in% span
  ids_v <- ids0[in_sp]
  w_v   <- w0[in_sp]

  if (length(ids_v) == 0) {
    out <- data.frame(
      id           = character(0),
      n_present    = integer(0),
      n_missing    = integer(0),
      prop_present = numeric(0),
      has_lower    = logical(0),
      has_upper    = logical(0),
      n_gap        = integer(0),
      max_gap      = integer(0),
      stringsAsFactors = FALSE
    )
    names(out)[1] <- id_col
    return(out)
  }

  dd    <- .weasel_dedup_index(ids_v, w_v)
  ids_v <- ids_v[dd$idx]
  w_v   <- w_v[dd$idx]

  pos <- match(w_v, span)
  out <- .weasel_gap_metrics(ids_v, pos, L)

  out$n_missing    <- L - out$n_present
  out$prop_present <- out$n_present / L
  out <- out[c("id", "n_present", "n_missing", "prop_present",
               "has_lower", "has_upper", "n_gap", "max_gap")]
  names(out)[1] <- id_col
  rownames(out) <- NULL
  out
}

# choose the analysis window; "core" slides a window of min_len
# consecutive grid waves over the full grid and picks the one with the
# highest mean per-wave respondent coverage (with grid = "consecutive",
# unobserved waves inside the span count as zero coverage)
#' @noRd
.weasel_choose_span <- function(data, id_col, wave_col,
                                span = c("core", "full"), min_len = 6L,
                                grid = c("consecutive", "observed")) {
  span <- match.arg(span)
  grid <- match.arg(grid)
  waves <- .weasel_check_wave(data[[wave_col]], wave_col)
  if (length(waves) < 3) .weasel_stop("need at least 3 distinct waves.")

  lower_full <- min(waves)
  upper_full <- max(waves)
  grid_full  <- .weasel_grid_waves(waves, lower_full, upper_full, grid)

  if (span == "full") {
    if (grid == "consecutive") .weasel_warn_empty_span(grid_full, waves)
    return(list(lower = lower_full, upper = upper_full,
                span = grid_full, reason = "full", candidates = NULL))
  }

  W <- length(grid_full)
  L <- min(max(as.integer(min_len), 2L), W)

  ok   <- !is.na(data[[id_col]]) & !is.na(data[[wave_col]])
  ids0 <- data[[id_col]][ok]
  w0   <- as.integer(round(data[[wave_col]][ok]))
  dd   <- .weasel_dedup_index(ids0, w0)
  cov  <- tabulate(match(w0[dd$idx], grid_full), nbins = W)
  cs   <- c(0L, cumsum(cov))
  win <- cs[(L + 1L):(W + 1L)] - cs[seq_len(W - L + 1L)]
  best_i <- which.max(win)
  n_tied <- sum(win == win[best_i])

  # every candidate window with its objective value, so the selection
  # is inspectable instead of a silent argmax
  starts <- seq_len(W - L + 1L)
  candidates <- data.frame(
    lower    = grid_full[starts],
    upper    = grid_full[starts + L - 1L],
    coverage = as.integer(win),
    chosen   = starts == best_i,
    stringsAsFactors = FALSE
  )

  chosen <- grid_full[best_i:(best_i + L - 1L)]
  if (n_tied > 1L) {
    .weasel_warn(
      n_tied, " candidate windows tie on coverage (", win[best_i],
      " respondent-wave observations); the earliest, ", chosen[1L], ":",
      chosen[L], ", is used. inspect $span_candidates, or set explicit ",
      "lower/upper bounds to pick another window.",
      class = "weasel_tied_windows"
    )
  }
  if (grid == "consecutive") .weasel_warn_empty_span(chosen, waves)
  list(lower = chosen[1L], upper = chosen[L], span = chosen,
       reason = "core", candidates = candidates)
}

# ---- exported planning functions ------------------------------------------

#' Match a scenario name (possibly abbreviated) to available choices
#'
#' Used internally by [weasel_apply()] and [weasel_summarize_subset()],
#' but also useful when building custom workflows. Accepts either an
#' exact name or an unambiguous prefix; arbitrary substrings are not
#' matched, so an abbreviation always selects the scenario it visibly
#' starts.
#'
#' @param scenario Character string to look up.
#' @param choices Character vector of valid scenario names.
#'
#' @return The matched scenario name.
#'
#' @examples
#' choices <- c("anchored_strict", "anchored_balanced", "lenient_info_max")
#'
#' weasel_match_scenario("anchored_strict", choices)
#' weasel_match_scenario("lenient", choices)  # unique prefix
#'
#' # ambiguous prefix errors: "anchored" starts two scenarios
#' try(weasel_match_scenario("anchored", choices))
#'
#' @export
weasel_match_scenario <- function(scenario, choices) {
  scenario <- as.character(scenario)[1]
  choices  <- as.character(choices)
  if (scenario %in% choices) return(scenario)
  hits <- choices[startsWith(choices, scenario)]
  if (length(hits) == 1) return(hits)
  .weasel_stop(
    "scenario not found or ambiguous: '", scenario,
    "' (use an exact name or a unique prefix). Available: ",
    paste(choices, collapse = ", "),
    class = "weasel_error_scenario"
  )
}

#' Score and rank scenarios from a weasel plan
#'
#' Computes a composite score for each scenario from *observed* subset
#' quality, never from the scenario's configured tolerances. With the
#' default weights the score is
#'
#' \deqn{2 \cdot coverage + 1.2 \cdot endpoints + 0.8 \cdot size
#'   - 0.6 \cdot missing - 0.4 \cdot gaps}
#'
#' where `coverage` is `mean_prop_present`, `endpoints` is
#' `endpoint_rate`, `size` is `n_ids` divided by the largest `n_ids`
#' across scenarios, `missing` is `worst_missing / L` (the largest
#' per-respondent count of missing waves among retained respondents,
#' normalised by span length), and `gaps` is
#' `(mean_n_gap + mean_max_gap) / L`. Scenarios retaining no
#' respondents receive `NA` and are never recommended. Ties are broken
#' in favour of the larger sample.
#'
#' The score is a configurable heuristic, not a validated decision
#' rule: the weights encode one reasonable trade-off, and the
#' `recommended` flag marks the highest score under the declared
#' weights, nothing more. Because the `size` term is normalised by the
#' largest `n_ids` in the supplied set, a scenario's score depends on
#' which scenarios it is compared with; scores are comparison-relative,
#' not absolute properties of a scenario. Scenarios whose scores fall
#' within `tie_tolerance` of the best are flagged in `near_tie`,
#' signalling that the heuristic does not meaningfully distinguish
#' them; endpoint-requiring scenarios earn the `endpoints` term by
#' construction, which is worth keeping in mind when comparing anchored
#' with unanchored scenarios.
#'
#' @param plan_obj A list with element `plan` (data frame of scenarios),
#'   as returned by [weasel_plan()].
#' @param weights Named numeric vector overriding any of the default
#'   weights `c(coverage = 2, endpoints = 1.2, size = 0.8,
#'   missing = 0.6, gaps = 0.4)`. `NA` values are rejected.
#' @param tie_tolerance Single non-negative number; scenarios whose
#'   scores are within this distance of the best score are flagged as
#'   `near_tie` (only when at least two qualify).
#'
#' @return The plan data frame with added `score`, `recommended`, and
#'   `near_tie` columns. The active weights are attached as attribute
#'   `"weights"` and the per-scenario score decomposition as attribute
#'   `"score_components"`, so every recommendation can be audited.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' cmp <- weasel_compare_scenarios(p)
#' weasel_print_table(cmp, title = "Scenario comparison")
#'
#' # emphasise sample size more strongly
#' cmp2 <- weasel_compare_scenarios(p, weights = c(size = 2))
#' cmp2$scenario[cmp2$recommended]
#'
#' # audit the recommendation: active weights and score decomposition
#' attr(cmp, "weights")
#' attr(cmp, "score_components")
#'
#' @export
weasel_compare_scenarios <- function(plan_obj, weights = NULL,
                                     tie_tolerance = 0.01) {
  p <- plan_obj$plan
  if (!is.data.frame(p)) .weasel_stop("plan_obj$plan must be a data.frame.")

  w <- c(coverage = 2, endpoints = 1.2, size = 0.8,
         missing = 0.6, gaps = 0.4)
  if (!is.null(weights)) {
    wn <- names(weights)
    if (is.null(wn) || !all(wn %in% names(w))) {
      .weasel_stop("weights must be a named vector using names: ",
                   paste(names(w), collapse = ", "))
    }
    weights <- suppressWarnings(as.numeric(weights))
    if (anyNA(weights)) .weasel_stop("weights must not contain NA.")
    w[wn] <- weights
  }
  tie_tolerance <- suppressWarnings(as.numeric(tie_tolerance[1]))
  if (is.na(tie_tolerance) || tie_tolerance < 0) {
    .weasel_stop("tie_tolerance must be a single non-negative number.")
  }

  num_cols <- c("mean_prop_present", "endpoint_rate", "worst_missing",
                "mean_n_gap", "mean_max_gap", "L", "n_ids")
  for (nm in intersect(num_cols, names(p))) p[[nm]] <- as.numeric(p[[nm]])

  max_n <- max(p$n_ids, 0, na.rm = TRUE)
  size_term <- if (max_n > 0) p$n_ids / max_n else 0

  p$score <- w[["coverage"]]  * p$mean_prop_present +
    w[["endpoints"]] * p$endpoint_rate +
    w[["size"]]      * size_term -
    w[["missing"]]   * (p$worst_missing / pmax(p$L, 1)) -
    w[["gaps"]]      * ((p$mean_n_gap + p$mean_max_gap) / pmax(p$L, 1))

  p$recommended <- FALSE
  rankable <- which(!is.na(p$score) & p$n_ids > 0)
  if (length(rankable) > 0) {
    best <- rankable[order(-p$score[rankable], -p$n_ids[rankable])][1]
    p$recommended[best] <- TRUE
  }

  # flag practical ties: scenarios whose scores are within tie_tolerance
  # of the best are not meaningfully distinguished by the heuristic
  p$near_tie <- FALSE
  if (length(rankable) > 0) {
    best_score <- max(p$score[rankable])
    close_i <- rankable[best_score - p$score[rankable] <= tie_tolerance]
    if (length(close_i) >= 2) p$near_tie[close_i] <- TRUE
  }

  # expose the active weights and the per-scenario score decomposition,
  # so the heuristic is auditable rather than a sealed number
  attr(p, "weights") <- w
  attr(p, "score_components") <- data.frame(
    scenario  = p$scenario,
    coverage  = w[["coverage"]] * p$mean_prop_present,
    endpoints = w[["endpoints"]] * p$endpoint_rate,
    size      = w[["size"]] * size_term,
    missing   = -w[["missing"]] * (p$worst_missing / pmax(p$L, 1)),
    gaps      = -w[["gaps"]] * ((p$mean_n_gap + p$mean_max_gap) / pmax(p$L, 1)),
    stringsAsFactors = FALSE
  )

  p
}

#' Summarize scenario comparison as a sentence
#'
#' Produces a human-readable sentence from the scored scenario table,
#' suitable for reports or console output. Intended to be called right
#' after [weasel_compare_scenarios()]. When no scenario is recommended
#' (for example because every scenario retains zero respondents), the
#' sentence says so instead of naming one.
#'
#' @param cmp Data frame returned by [weasel_compare_scenarios()].
#' @param digits Number of decimal places.
#'
#' @return A single character string.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' cmp <- weasel_compare_scenarios(p)
#' cat(weasel_compare_to_sentence(cmp), "\n")
#'
#' @export
weasel_compare_to_sentence <- function(cmp, digits = 3) {
  if (!inherits(cmp, "data.frame")) .weasel_stop("cmp must be a data.frame.")
  need <- c("scenario", "n_ids", "mean_prop_present",
            "endpoint_rate", "recommended")
  if (!all(need %in% names(cmp))) {
    .weasel_stop("cmp is missing required columns.")
  }

  rec_i <- which(cmp$recommended)
  lines <- if (length(rec_i) == 0) {
    # never fabricate a recommendation: say plainly that none exists
    if (all(cmp$n_ids == 0)) {
      "No scenario is recommended: every scenario retains zero respondents."
    } else {
      "No scenario is marked as recommended."
    }
  } else {
    rec <- cmp[rec_i[1], , drop = FALSE]
    paste0("Recommended scenario (highest composite score under the ",
           "declared weights): ", dQuote(rec$scenario, FALSE), ".")
  }
  for (i in seq_len(nrow(cmp))) {
    r <- cmp[i, , drop = FALSE]
    lines <- c(lines, sprintf(
      "%s keeps %s respondent(s), coverage %s, endpoints %s.",
      dQuote(r$scenario, FALSE),
      r$n_ids,
      .weasel_format_num(as.numeric(r$mean_prop_present), digits),
      .weasel_format_num(as.numeric(r$endpoint_rate), digits)
    ))
  }
  if (!is.null(cmp$near_tie) && sum(cmp$near_tie, na.rm = TRUE) >= 2) {
    lines <- c(lines, paste0(
      "Note: ", sum(cmp$near_tie, na.rm = TRUE), " scenarios score ",
      "within the tie tolerance; the recommendation is not unique on ",
      "the score alone."
    ))
  }
  paste(lines, collapse = " ")
}

#' Build a scenario-based selection plan
#'
#' Evaluates three default scenarios (`anchored_strict`,
#' `anchored_balanced`, `lenient_info_max`) or a validated custom
#' scenario table against the data, computing per-scenario respondent
#' counts and observed quality metrics. Gap constraints refer to
#' interior gaps: runs of missing grid waves strictly between a
#' respondent's first and last observed wave inside the span; missing
#' endpoints are handled separately through `require_endpoints`.
#'
#' @param data A long-format data frame. A respondent is considered
#'   observed at a wave if a row with that (id, wave) pair exists.
#'   Duplicated (id, wave) rows are counted once and trigger a warning.
#' @param id Name of the respondent-identifier column. Any atomic type
#'   is supported.
#' @param wave Name of the wave/time column. Must be numeric with
#'   integer-valued entries.
#' @param span Either `"core"` (window of `core_len` consecutive grid
#'   waves with the highest total respondent-wave coverage; exact
#'   coverage ties are resolved in favour of the earliest window with a
#'   classed warning, `weasel_tied_windows`, and every candidate window
#'   is stored in the returned `span_candidates` table) or `"full"`
#'   (all waves). Ignored when explicit `lower`/`upper` bounds are
#'   supplied; supplying both raises an error.
#' @param core_len Integer; desired window length when `span = "core"`.
#' @param lower,upper Optional explicit integer window bounds. When
#'   either is supplied the analysis window is fixed a priori
#'   (`span_reason = "explicit"`), which the justification text reports
#'   as a design decision rather than an automatic selection. Bounds
#'   are interpreted on the chosen `grid`; the effective bounds are the
#'   first and last grid waves inside the requested range.
#' @param scenarios Optional data frame of custom scenarios with the
#'   columns `scenario`, `require_endpoints`, `max_missing`,
#'   `n_gap_max`, `max_gap_len`. The table is validated; missing
#'   columns raise an error. The pre-0.4 column name `max_gap_max` is
#'   still accepted with a deprecation warning (class
#'   `weasel_deprecated`).
#' @param grid How the wave grid inside the span is defined.
#'   `"consecutive"` (default) treats every integer between the span
#'   bounds as a scheduled wave; `"observed"` uses only wave values
#'   that occur anywhere in the data, which is the right choice for
#'   biennial and other non-consecutive schedules. With
#'   `"consecutive"`, a warning (class `weasel_empty_waves`) is issued
#'   when the chosen span contains waves that no respondent has, since
#'   such waves count as missed by everyone.
#' @param keep_data If `TRUE` (default), the original data is attached
#'   to the returned object so that [weasel_apply()],
#'   [weasel_summarize_subset()], and [weasel_selectivity()] can reuse
#'   it. Set to `FALSE` to keep the plan object small (for example
#'   before saving it with `saveRDS()`); those functions then need the
#'   data passed back in through their `data` argument.
#'
#' @return A list of class `weasel_plan` with elements `plan` (scored
#'   scenario table), `id_metrics`, `lower`, `upper`, `span` (integer
#'   vector of grid waves), `grid`, `span_reason` (`"core"`, `"full"`,
#'   or `"explicit"`), `span_candidates` (for core spans, every
#'   candidate window with its coverage and a `chosen` flag; `NULL`
#'   otherwise), `population` (the planning denominator: rows and
#'   distinct ids in the data, ids and unique pairs observed in the
#'   span; all retention proportions are relative to
#'   `"observed_in_span"`), `fingerprint` (a structural fingerprint of
#'   the data used to detect mismatched reunions later), `id`, and
#'   `wave`. When `keep_data = TRUE` the original `data` is attached as
#'   an attribute. Printing the object shows a compact summary instead
#'   of the raw list.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, id = "id", wave = "time", span = "core")
#' p
#'
#' cmp <- weasel_compare_scenarios(p)
#' weasel_print_table(cmp, title = "Scenario overview")
#'
#' sub <- weasel_apply(p, "anchored_balanced")
#' dim(sub)
#'
#' # biennial schedule: evaluate presence on the observed grid
#' b <- generate_weasel_dummy_data(n_ids = 60, waves = seq(2008, 2020, 2),
#'                                 seed = 1)
#' pb <- weasel_plan(b, "id", "time", span = "full", grid = "observed")
#'
#' # a design-determined window: fix the bounds a priori
#' pe <- weasel_plan(d, "id", "time", lower = 3, upper = 8)
#' pe$span_reason
#'
#' @export
weasel_plan <- function(data,
                        id,
                        wave,
                        span = c("core", "full"),
                        core_len = 6L,
                        scenarios = NULL,
                        grid = c("consecutive", "observed"),
                        keep_data = TRUE,
                        lower = NULL,
                        upper = NULL) {
  span_given <- !missing(span)
  .weasel_check_id_wave(data, id, wave)
  span <- match.arg(span)
  grid <- match.arg(grid)
  core_len <- .weasel_or(.weasel_check_count(core_len, "core_len"), 6L)
  lower <- .weasel_check_bound(lower, "lower")
  upper <- .weasel_check_bound(upper, "upper")
  .weasel_check_duplicates(
    data[!is.na(data[[id]]) & !is.na(data[[wave]]), c(id, wave), drop = FALSE],
    id, wave
  )

  if (!is.null(lower) || !is.null(upper)) {
    # explicit a-priori window: recorded as such, so justification text
    # never attributes the window to an automatic rule
    if (span_given) {
      .weasel_stop("supply either span = \"core\"/\"full\" or explicit ",
                   "lower/upper bounds, not both.")
    }
    waves_all <- .weasel_check_wave(data[[wave]], wave)
    lo <- .weasel_or(lower, min(waves_all))
    up <- .weasel_or(upper, max(waves_all))
    if (up < lo) .weasel_stop("upper must be >= lower.")
    span_vec0 <- .weasel_grid_waves(waves_all, lo, up, grid)
    if (grid == "consecutive") .weasel_warn_empty_span(span_vec0, waves_all)
    span_pick <- list(lower = span_vec0[1L],
                      upper = span_vec0[length(span_vec0)],
                      span = span_vec0, reason = "explicit",
                      candidates = NULL)
  } else {
    span_pick <- .weasel_choose_span(data, id, wave,
                                     span = span,
                                     min_len = core_len,
                                     grid = grid)
  }
  lower       <- span_pick$lower
  upper       <- span_pick$upper
  span_vec    <- span_pick$span
  span_reason <- span_pick$reason
  L           <- length(span_vec)

  .weasel_report_dropped(data, id, wave, span_vec)

  idm <- .weasel_id_metrics(data, id, wave, span_vec)
  if (nrow(idm) == 0) .weasel_stop("no usable ids found in the chosen span.")

  if (is.null(scenarios)) {
    scenarios <- data.frame(
      scenario          = c("anchored_strict", "anchored_balanced",
                            "lenient_info_max"),
      require_endpoints = c(TRUE, TRUE, FALSE),
      max_missing       = c(0, 1, 2),
      n_gap_max         = c(0, 1, 2),
      max_gap_len       = c(0, 1, 2),
      stringsAsFactors  = FALSE
    )
  }
  scenarios <- .weasel_check_scenarios(scenarios)

  keep_masks <- lapply(seq_len(nrow(scenarios)), function(i) {
    keep <- idm$n_missing <= scenarios$max_missing[i] &
      idm$n_gap <= scenarios$n_gap_max[i] &
      idm$max_gap <= scenarios$max_gap_len[i]
    if (isTRUE(scenarios$require_endpoints[i])) {
      keep <- keep & idm$has_lower & idm$has_upper
    }
    keep
  })
  ids_list <- lapply(keep_masks, function(k) idm[[id]][k])

  plan       <- scenarios
  plan$ids   <- ids_list
  plan$n_ids <- vapply(keep_masks, sum, integer(1))

  stat_for <- function(col, fun = mean) {
    vapply(keep_masks, function(k) {
      v <- idm[[col]][k]
      if (length(v) == 0) NA_real_ else as.numeric(fun(v))
    }, numeric(1))
  }

  plan$mean_present      <- stat_for("n_present")
  plan$mean_prop_present <- stat_for("prop_present")
  plan$endpoint_rate     <- vapply(keep_masks, function(k) {
    v <- (idm$has_lower & idm$has_upper)[k]
    if (length(v) == 0) NA_real_ else mean(v)
  }, numeric(1))
  plan$mean_n_gap    <- stat_for("n_gap")
  plan$mean_max_gap  <- stat_for("max_gap")
  plan$worst_missing <- stat_for("n_missing", fun = max)

  plan$lower       <- lower
  plan$upper       <- upper
  plan$L           <- L
  plan$span_reason <- span_reason

  default_notes <- c(
    anchored_strict   = "cleanest panel, smallest N",
    anchored_balanced = "good balance, anchored endpoints",
    lenient_info_max  = "largest N, endpoints not guaranteed"
  )
  plan$note <- unname(default_notes[plan$scenario])
  plan$note[is.na(plan$note)] <- ""

  plan <- weasel_compare_scenarios(list(plan = plan))

  grid_txt <- if (grid == "observed") {
    paste0(L, " observed waves")
  } else {
    paste0("L = ", L)
  }
  .weasel_ok("plan ready: span ", lower, ":", upper,
             " (", span_reason, ", ", grid_txt, ")")

  ids_nonmissing <- data[[id]][!is.na(data[[id]])]
  population <- list(
    denominator     = "observed_in_span",
    n_rows_data     = nrow(data),
    n_ids_data      = length(unique(ids_nonmissing)),
    n_ids_in_span   = nrow(idm),
    n_pairs_in_span = as.integer(sum(idm$n_present))
  )

  obj <- list(
    plan            = plan,
    id_metrics      = idm,
    lower           = lower,
    upper           = upper,
    span            = span_vec,
    grid            = grid,
    span_reason     = span_reason,
    span_candidates = span_pick$candidates,
    population      = population,
    fingerprint     = .weasel_data_fingerprint(data, id, wave),
    id              = id,
    wave            = wave
  )
  if (isTRUE(keep_data)) attr(obj, "data") <- data
  class(obj) <- "weasel_plan"
  obj
}

#' Print a weasel plan
#'
#' Compact display of a plan object: the chosen span, the number of
#' respondents observed in it, and the scored scenario table. The
#' attached data (if any) and the per-scenario id lists are summarised
#' by size instead of being printed in full.
#'
#' @param x Object returned by [weasel_plan()].
#' @param digits Integer; decimal places for numeric columns.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 60, n_times = 8, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' p
#'
#' @export
print.weasel_plan <- function(x, digits = 3, ...) {
  # exact indexing: $span would partial-match span_reason on legacy objects
  sp <- .weasel_or(x[["span"]], .weasel_seq_int(x$lower, x$upper))
  grid_txt <- if (identical(x$grid, "observed")) {
    paste0(length(sp), " observed waves")
  } else {
    paste0("L = ", length(sp))
  }
  cat("<weasel_plan>\n")
  cat("  span: ", x$lower, ":", x$upper, " (",
      .weasel_or(x$span_reason, "?"), ", ", grid_txt, ")\n", sep = "")
  cat("  respondents observed in span: ", nrow(x$id_metrics), "\n", sep = "")
  pop <- x[["population"]]
  if (!is.null(pop)) {
    cat("  population: ", pop$n_ids_in_span, " of ", pop$n_ids_data,
        " distinct ids observed in span (denominator: ",
        pop$denominator, ")\n", sep = "")
  }
  cand <- x[["span_candidates"]]
  if (!is.null(cand)) {
    n_tied <- sum(cand$coverage == max(cand$coverage))
    if (n_tied > 1L) {
      cat("  note: earliest of ", n_tied, " coverage-tied windows ",
          "(see $span_candidates)\n", sep = "")
    }
  }

  cols <- intersect(c("scenario", "n_ids", "mean_prop_present",
                      "endpoint_rate", "score", "recommended", "near_tie"),
                    names(x$plan))
  tab <- x$plan[cols]
  if ("near_tie" %in% names(tab) && !any(tab$near_tie)) {
    tab$near_tie <- NULL
  }
  for (nm in names(tab)) tab[[nm]] <- .weasel_maybe_round(tab[[nm]], digits)
  print(tab, row.names = FALSE)

  d <- attr(x, "data")
  if (is.null(d)) {
    cat("  data: not attached (created with keep_data = FALSE)\n")
  } else {
    cat("  data: ", nrow(d), " row(s) attached\n", sep = "")
  }
  invisible(x)
}

#' Apply a scenario to obtain filtered long-format data
#'
#' Subsets the original data to the respondents and wave range selected
#' by a given scenario. This is typically the final step: after
#' reviewing [weasel_compare_scenarios()], pick a scenario and call this
#' to get the analysis-ready data frame. Selection metrics count each
#' (id, wave) pair once, but output rows are returned as they appear in
#' the data; if duplicated pairs remain in the result, a classed warning
#' (`weasel_duplicates`) reminds you to resolve them before modelling.
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param scenario Name (or unambiguous abbreviation) of the scenario.
#' @param data Optional long-format data frame; defaults to the data
#'   attached to `plan_obj`. Required when the plan was created with
#'   `keep_data = FALSE`. Explicitly supplied data are compared with
#'   the plan's structural fingerprint; a mismatch triggers a classed
#'   warning (`weasel_data_mismatch`), since the plan's id lists were
#'   computed on the original data.
#'
#' @return A data frame.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#'
#' balanced <- weasel_apply(p, "anchored_balanced")
#' dim(balanced)
#'
#' # unambiguous prefixes work
#' strict <- weasel_apply(p, "anchored_s")
#' dim(strict)
#'
#' # plans built with keep_data = FALSE need the data passed back in
#' p2 <- weasel_plan(d, "id", "time", span = "core", keep_data = FALSE)
#' nrow(weasel_apply(p2, "lenient", data = d))
#'
#' @export
weasel_apply <- function(plan_obj, scenario, data = NULL) {
  .weasel_check_plan(plan_obj)
  scenario <- weasel_match_scenario(scenario, plan_obj$plan$scenario)
  row <- plan_obj$plan[plan_obj$plan$scenario == scenario, , drop = FALSE]
  if (nrow(row) != 1) {
    .weasel_stop("scenario not found or ambiguous.",
                 class = "weasel_error_scenario")
  }

  user_data <- !is.null(data)
  if (is.null(data)) data <- attr(plan_obj, "data")
  if (is.null(data)) {
    .weasel_stop("no data available: the plan was created with ",
                 "keep_data = FALSE; pass the original data via the ",
                 "'data' argument.")
  }
  id_col   <- plan_obj$id
  wave_col <- plan_obj$wave
  .weasel_check_id_wave(data, id_col, wave_col)
  if (user_data) .weasel_check_fingerprint(plan_obj, data, id_col, wave_col)

  ids_keep <- row$ids[[1]]
  sp    <- .weasel_plan_span(plan_obj, row)
  w_int <- as.integer(round(data[[wave_col]]))

  out <- data[data[[id_col]] %in% ids_keep & (w_int %in% sp), , drop = FALSE]
  .weasel_warn_output_duplicates(out, id_col, wave_col)
  out
}

#' Summarize a chosen scenario subset
#'
#' Computes headline statistics, per-wave coverage, and a missingness
#' distribution for a specific scenario. Use this to audit a scenario
#' before committing to [weasel_apply()]. By default the data and
#' column names stored in the plan object are reused; supply `data`,
#' `id`, or `wave` only to override them. `headline$n_rows` counts
#' physical rows; when duplicated (id, wave) rows are present it
#' therefore exceeds the deduplicated pair count, and a classed warning
#' (`weasel_duplicates`) is emitted.
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param scenario Name of the scenario to summarize.
#' @param data Optional long-format data frame; defaults to the data
#'   attached to `plan_obj`. Required when the plan was created with
#'   `keep_data = FALSE`. Explicitly supplied data are checked against
#'   the plan's structural fingerprint (classed warning
#'   `weasel_data_mismatch` on mismatch).
#' @param id Optional id column name; defaults to `plan_obj$id`.
#' @param wave Optional wave column name; defaults to `plan_obj$wave`.
#' @param digits Number of decimal places for the sentence output.
#'
#' @return A list with elements `headline`, `per_wave_coverage`,
#'   `missing_distribution`, `data`, and `sentence`.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#'
#' s <- weasel_summarize_subset(p, "anchored_balanced")
#' weasel_print_table(s$headline, title = "Headline")
#' weasel_print_table(s$per_wave_coverage, title = "Per-wave coverage")
#' cat(weasel_subset_to_sentence(s), "\n")
#'
#' @export
weasel_summarize_subset <- function(plan_obj, scenario, data = NULL,
                                    id = NULL, wave = NULL, digits = 3) {
  .weasel_check_plan(plan_obj)

  user_data <- !is.null(data)
  if (is.null(data)) data <- attr(plan_obj, "data")
  if (is.null(data)) {
    .weasel_stop("no data available: the plan was created with ",
                 "keep_data = FALSE; pass the original data via the ",
                 "'data' argument.")
  }
  if (is.null(id))   id   <- plan_obj$id
  if (is.null(wave)) wave <- plan_obj$wave
  .weasel_check_id_wave(data, id, wave)
  if (user_data) .weasel_check_fingerprint(plan_obj, data, id, wave)

  scenario <- weasel_match_scenario(scenario, plan_obj$plan$scenario)
  row <- plan_obj$plan[plan_obj$plan$scenario == scenario, , drop = FALSE]
  if (nrow(row) != 1) {
    .weasel_stop("scenario not found or ambiguous.",
                 class = "weasel_error_scenario")
  }

  ids_keep <- row$ids[[1]]
  lower    <- as.integer(row$lower[[1]])
  upper    <- as.integer(row$upper[[1]])
  sp       <- .weasel_plan_span(plan_obj, row)
  L        <- length(sp)
  if (length(ids_keep) == 0) {
    .weasel_stop("scenario '", scenario, "' retains no respondents.",
                 class = "weasel_error_empty_scenario")
  }

  w_all  <- as.integer(round(data[[wave]]))
  subdat <- data[data[[id]] %in% ids_keep & (w_all %in% sp), , drop = FALSE]
  .weasel_warn_output_duplicates(subdat, id, wave)

  id_wave <- subdat[c(id, wave)]
  id_wave[[wave]] <- as.integer(round(id_wave[[wave]]))
  id_wave <- unique(id_wave)

  per_wave <- data.frame(
    wave  = sp,
    n_ids = as.integer(table(factor(id_wave[[wave]], levels = sp))),
    stringsAsFactors = FALSE
  )

  waves_by_id <- split(id_wave[[wave]], id_wave[[id]])
  n_present   <- vapply(waves_by_id, function(w) length(unique(w)), integer(1))
  n_missing   <- L - n_present

  has_lower     <- vapply(waves_by_id, function(w) any(w == lower), logical(1))
  has_upper     <- vapply(waves_by_id, function(w) any(w == upper), logical(1))
  endpoint_rate <- mean(has_lower & has_upper)

  missing_dist <- as.data.frame(table(n_missing), stringsAsFactors = FALSE)
  names(missing_dist)    <- c("n_missing", "n_ids")
  missing_dist$n_missing <- as.integer(as.character(missing_dist$n_missing))
  missing_dist$n_ids     <- as.integer(missing_dist$n_ids)
  missing_dist <- missing_dist[order(missing_dist$n_missing), , drop = FALSE]
  rownames(missing_dist) <- NULL

  headline <- data.frame(
    scenario      = scenario,
    lower         = lower,
    upper         = upper,
    L             = L,
    n_ids         = length(ids_keep),
    n_rows        = nrow(subdat),
    mean_present  = mean(n_present),
    mean_missing  = mean(n_missing),
    endpoint_rate = endpoint_rate,
    min_present   = min(n_present),
    max_present   = max(n_present),
    stringsAsFactors = FALSE
  )

  weakest_i <- which.min(per_wave$n_ids)
  md_txt <- paste(
    sprintf("%s respondent(s) have %s missing wave(s)",
            missing_dist$n_ids, missing_dist$n_missing),
    collapse = "; "
  )

  L_txt <- if (identical(plan_obj$grid, "observed")) {
    paste0("L = ", L, " observed waves")
  } else {
    paste0("L = ", L)
  }

  sentence <- paste0(
    "Scenario ", dQuote(scenario, FALSE), " selects ", headline$n_ids,
    " respondent(s) and yields ", headline$n_rows,
    " row(s) in long format for waves ", lower, " to ", upper,
    " (", L_txt, "). ",
    "Mean observed waves: ",
    .weasel_format_num(headline$mean_present, digits),
    " (missing: ", .weasel_format_num(headline$mean_missing, digits), "). ",
    "Endpoint rate: ", .weasel_format_num(headline$endpoint_rate, digits),
    ". Missingness: ", md_txt, ". ",
    "Lowest wave coverage is wave ", per_wave$wave[weakest_i],
    " with ", per_wave$n_ids[weakest_i], " respondent(s)."
  )

  list(
    headline             = headline,
    per_wave_coverage    = per_wave,
    missing_distribution = missing_dist,
    data                 = subdat,
    sentence             = sentence
  )
}

#' Extract the sentence from a subset summary
#'
#' Convenience accessor for the natural-language summary built by
#' [weasel_summarize_subset()].
#'
#' @param s Object returned by [weasel_summarize_subset()].
#'
#' @return A character string.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' s <- weasel_summarize_subset(p, "anchored_strict")
#' cat(weasel_subset_to_sentence(s), "\n")
#'
#' @export
weasel_subset_to_sentence <- function(s) {
  if (!is.list(s) || is.null(s$sentence)) {
    .weasel_stop("s must be from weasel_summarize_subset().")
  }
  s$sentence
}
