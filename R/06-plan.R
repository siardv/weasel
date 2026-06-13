# scenario-based planning and comparison

# ---- internal helpers -----------------------------------------------------

# per-respondent presence metrics over the integer span lower:upper;
# ids keep their original type; gaps are interior gaps (runs of missing
# waves strictly between a respondent's first and last observed wave)
#' @noRd
.weasel_id_metrics <- function(data, id_col, wave_col, lower, upper) {
  span <- .weasel_seq_int(lower, upper)
  L    <- length(span)

  d <- data[!is.na(data[[id_col]]) & !is.na(data[[wave_col]]),
            c(id_col, wave_col), drop = FALSE]
  d <- d[d[[wave_col]] >= lower & d[[wave_col]] <= upper, , drop = FALSE]
  d <- unique(d)

  ids <- sort(unique(d[[id_col]]))
  if (length(ids) == 0) {
    out <- data.frame(
      id           = ids,
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

  present <- matrix(FALSE, nrow = length(ids), ncol = L)
  i <- match(d[[id_col]], ids)
  j <- match(as.integer(d[[wave_col]]), span)
  ok <- !is.na(i) & !is.na(j)
  present[cbind(i[ok], j[ok])] <- TRUE

  gaps <- apply(present, 1, .weasel_interior_gaps)

  out <- data.frame(
    id           = ids,
    n_present    = vapply(gaps, function(g) g$n_present, integer(1)),
    has_lower    = present[, 1],
    has_upper    = present[, L],
    n_gap        = vapply(gaps, function(g) g$n_gap, integer(1)),
    max_gap      = vapply(gaps, function(g) g$max_gap, integer(1)),
    stringsAsFactors = FALSE
  )
  out$n_missing    <- L - out$n_present
  out$prop_present <- out$n_present / L
  out <- out[c("id", "n_present", "n_missing", "prop_present",
               "has_lower", "has_upper", "n_gap", "max_gap")]
  names(out)[1] <- id_col
  rownames(out) <- NULL
  out
}

# choose the analysis window; "core" slides a window of min_len
# consecutive integer waves over the full integer span and picks the one
# with the highest mean per-wave respondent coverage (unobserved waves
# inside the span count as zero coverage)
#' @noRd
.weasel_choose_span <- function(data, id_col, wave_col,
                                span = c("core", "full"), min_len = 6L) {
  span  <- match.arg(span)
  waves <- .weasel_check_wave(data[[wave_col]], wave_col)
  if (length(waves) < 3) .weasel_stop("need at least 3 distinct waves.")

  lower_full <- min(waves)
  upper_full <- max(waves)
  if (span == "full") {
    return(list(lower = lower_full, upper = upper_full, reason = "full"))
  }

  span_full <- .weasel_seq_int(lower_full, upper_full)
  W <- length(span_full)
  L <- min(max(as.integer(min_len), 2L), W)

  d <- unique(data[!is.na(data[[id_col]]) & !is.na(data[[wave_col]]),
                   c(id_col, wave_col), drop = FALSE])
  cov <- as.integer(table(factor(as.integer(d[[wave_col]]),
                                 levels = span_full)))

  best_i <- 1L
  best_score <- -Inf
  for (i in seq_len(W - L + 1L)) {
    s <- mean(cov[i:(i + L - 1L)])
    if (s > best_score) {
      best_score <- s
      best_i <- i
    }
  }

  list(lower = span_full[best_i],
       upper = span_full[best_i + L - 1L],
       reason = "core")
}

# ---- exported planning functions ------------------------------------------

#' Match a scenario name (possibly abbreviated) to available choices
#'
#' Used internally by [weasel_apply()] and [weasel_summarize_subset()],
#' but also useful when building custom workflows. Accepts either an
#' exact name or an unambiguous substring.
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
#' weasel_match_scenario("lenient", choices)
#'
#' # ambiguous substring errors: "anchored" matches two scenarios
#' try(weasel_match_scenario("anchored", choices))
#'
#' @export
weasel_match_scenario <- function(scenario, choices) {
  scenario <- as.character(scenario)[1]
  choices  <- as.character(choices)
  if (scenario %in% choices) return(scenario)
  hits <- choices[grepl(scenario, choices, fixed = TRUE)]
  if (length(hits) == 1) return(hits)
  .weasel_stop(
    "scenario not found or ambiguous: '", scenario,
    "'. Available: ", paste(choices, collapse = ", ")
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
#' @param plan_obj A list with element `plan` (data frame of scenarios),
#'   as returned by [weasel_plan()].
#' @param weights Named numeric vector overriding any of the default
#'   weights `c(coverage = 2, endpoints = 1.2, size = 0.8,
#'   missing = 0.6, gaps = 0.4)`.
#'
#' @return The plan data frame with added `score` and `recommended`
#'   columns.
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
#' @export
weasel_compare_scenarios <- function(plan_obj, weights = NULL) {
  p <- plan_obj$plan
  if (!is.data.frame(p)) .weasel_stop("plan_obj$plan must be a data.frame.")

  w <- c(coverage = 2, endpoints = 1.2, size = 0.8,
         missing = 0.6, gaps = 0.4)
  if (!is.null(weights)) {
    if (is.null(names(weights)) || !all(names(weights) %in% names(w))) {
      .weasel_stop("weights must be a named vector using names: ",
                   paste(names(w), collapse = ", "))
    }
    w[names(weights)] <- as.numeric(weights)
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

  p
}

#' Summarize scenario comparison as a sentence
#'
#' Produces a human-readable sentence from the scored scenario table,
#' suitable for reports or console output. Intended to be called right
#' after [weasel_compare_scenarios()].
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

  rec_i <- which(cmp$recommended)[1]
  if (is.na(rec_i) || length(rec_i) == 0) rec_i <- 1L
  rec <- cmp[rec_i, , drop = FALSE]

  lines <- paste0("Recommended scenario: ", dQuote(rec$scenario, FALSE), ".")
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
  paste(lines, collapse = " ")
}

#' Build a scenario-based selection plan
#'
#' Evaluates three default scenarios (`anchored_strict`,
#' `anchored_balanced`, `lenient_info_max`) or a validated custom
#' scenario table against the data, computing per-scenario respondent
#' counts and observed quality metrics. Gap constraints refer to
#' interior gaps: runs of missing waves strictly between a respondent's
#' first and last observed wave inside the span; missing endpoints are
#' handled separately through `require_endpoints`.
#'
#' @param data A long-format data frame. A respondent is considered
#'   observed at a wave if a row with that (id, wave) pair exists.
#' @param id Name of the respondent-identifier column. Any atomic type
#'   is supported.
#' @param wave Name of the wave/time column. Must be numeric with
#'   integer-valued entries.
#' @param span Either `"core"` (highest-coverage window of `core_len`
#'   consecutive waves) or `"full"` (all waves).
#' @param core_len Integer; desired window length when `span = "core"`.
#' @param scenarios Optional data frame of custom scenarios with the
#'   columns `scenario`, `require_endpoints`, `max_missing`,
#'   `n_gap_max`, `max_gap_max`. The table is validated; missing
#'   columns raise an error.
#'
#' @return A list with elements `plan` (scored scenario table),
#'   `id_metrics`, `lower`, `upper`, `span_reason`, `id`, and `wave`.
#'   The original `data` is attached as an attribute.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 100, n_times = 10, seed = 1)
#' p <- weasel_plan(d, id = "id", wave = "time", span = "core")
#'
#' cmp <- weasel_compare_scenarios(p)
#' weasel_print_table(cmp, title = "Scenario overview")
#'
#' sub <- weasel_apply(p, "anchored_balanced")
#' dim(sub)
#'
#' @export
weasel_plan <- function(data,
                        id,
                        wave,
                        span = c("core", "full"),
                        core_len = 6L,
                        scenarios = NULL) {
  .weasel_check_id_wave(data, id, wave)
  span <- match.arg(span)

  span_pick <- .weasel_choose_span(data, id, wave,
                                   span = span,
                                   min_len = as.integer(core_len))
  lower       <- span_pick$lower
  upper       <- span_pick$upper
  span_reason <- span_pick$reason
  L           <- length(.weasel_seq_int(lower, upper))

  idm <- .weasel_id_metrics(data, id, wave, lower, upper)
  if (nrow(idm) == 0) .weasel_stop("no usable ids found in the chosen span.")

  if (is.null(scenarios)) {
    scenarios <- data.frame(
      scenario          = c("anchored_strict", "anchored_balanced",
                            "lenient_info_max"),
      require_endpoints = c(TRUE, TRUE, FALSE),
      max_missing       = c(0, 1, 2),
      n_gap_max         = c(0, 1, 2),
      max_gap_max       = c(0, 1, 2),
      stringsAsFactors  = FALSE
    )
  }
  scenarios <- .weasel_check_scenarios(scenarios)

  build_ids <- function(require_endpoints, max_missing, n_gap_max,
                        max_gap_max) {
    keep <- idm$n_missing <= max_missing &
      idm$n_gap <= n_gap_max &
      idm$max_gap <= max_gap_max
    if (isTRUE(require_endpoints)) {
      keep <- keep & idm$has_lower & idm$has_upper
    }
    idm[[id]][keep]
  }

  ids_list <- lapply(seq_len(nrow(scenarios)), function(i) {
    build_ids(
      scenarios$require_endpoints[i],
      scenarios$max_missing[i],
      scenarios$n_gap_max[i],
      scenarios$max_gap_max[i]
    )
  })

  plan       <- scenarios
  plan$ids   <- ids_list
  plan$n_ids <- vapply(ids_list, length, integer(1))

  stat_for <- function(col, fun = mean) {
    vapply(ids_list, function(ids) {
      v <- idm[[col]][idm[[id]] %in% ids]
      if (length(v) == 0) NA_real_ else as.numeric(fun(v))
    }, numeric(1))
  }

  plan$mean_present      <- stat_for("n_present")
  plan$mean_prop_present <- stat_for("prop_present")
  plan$endpoint_rate     <- vapply(ids_list, function(ids) {
    v <- (idm$has_lower & idm$has_upper)[idm[[id]] %in% ids]
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

  .weasel_ok("plan ready: span ", lower, ":", upper,
             " (", span_reason, ", L = ", L, ")")

  obj <- list(
    plan        = plan,
    id_metrics  = idm,
    lower       = lower,
    upper       = upper,
    span_reason = span_reason,
    id          = id,
    wave        = wave
  )
  attr(obj, "data") <- data
  obj
}

#' Apply a scenario to obtain filtered long-format data
#'
#' Subsets the original data to the respondents and wave range selected
#' by a given scenario. This is typically the final step: after
#' reviewing [weasel_compare_scenarios()], pick a scenario and call this
#' to get the analysis-ready data frame.
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param scenario Name (or unambiguous abbreviation) of the scenario.
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
#' # abbreviations work when unambiguous
#' strict <- weasel_apply(p, "strict")
#' dim(strict)
#'
#' @export
weasel_apply <- function(plan_obj, scenario) {
  if (!is.list(plan_obj) || is.null(plan_obj$plan)) {
    .weasel_stop("plan_obj must be from weasel_plan().")
  }
  scenario <- weasel_match_scenario(scenario, plan_obj$plan$scenario)
  row <- plan_obj$plan[plan_obj$plan$scenario == scenario, , drop = FALSE]
  if (nrow(row) != 1) .weasel_stop("scenario not found or ambiguous.")

  data <- attr(plan_obj, "data")
  if (is.null(data)) {
    .weasel_stop("plan_obj has no attached data; recreate with weasel_plan().")
  }

  ids_keep <- row$ids[[1]]
  id_col   <- plan_obj$id
  wave_col <- plan_obj$wave
  lower    <- as.integer(row$lower[[1]])
  upper    <- as.integer(row$upper[[1]])

  data[data[[id_col]] %in% ids_keep &
         !is.na(data[[wave_col]]) &
         data[[wave_col]] >= lower &
         data[[wave_col]] <= upper, , drop = FALSE]
}

#' Summarize a chosen scenario subset
#'
#' Computes headline statistics, per-wave coverage, and a missingness
#' distribution for a specific scenario. Use this to audit a scenario
#' before committing to [weasel_apply()]. By default the data and
#' column names stored in the plan object are reused; supply `data`,
#' `id`, or `wave` only to override them.
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param scenario Name of the scenario to summarize.
#' @param data Optional long-format data frame; defaults to the data
#'   attached to `plan_obj`.
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
  if (!is.list(plan_obj) || is.null(plan_obj$plan)) {
    .weasel_stop("plan_obj must be from weasel_plan().")
  }
  if (!is.data.frame(plan_obj$plan)) {
    .weasel_stop("plan_obj$plan must be a data.frame.")
  }

  if (is.null(data)) data <- attr(plan_obj, "data")
  if (is.null(id))   id   <- plan_obj$id
  if (is.null(wave)) wave <- plan_obj$wave
  .weasel_check_id_wave(data, id, wave)

  scenario <- weasel_match_scenario(scenario, plan_obj$plan$scenario)
  row <- plan_obj$plan[plan_obj$plan$scenario == scenario, , drop = FALSE]
  if (nrow(row) != 1) .weasel_stop("scenario not found or ambiguous.")

  ids_keep <- row$ids[[1]]
  lower    <- as.integer(row$lower[[1]])
  upper    <- as.integer(row$upper[[1]])
  L        <- as.integer(row$L[[1]])
  if (length(ids_keep) == 0) {
    .weasel_stop("scenario '", scenario, "' retains no respondents.")
  }

  subdat <- data[data[[id]] %in% ids_keep &
                   !is.na(data[[wave]]) &
                   data[[wave]] >= lower &
                   data[[wave]] <= upper, , drop = FALSE]

  id_wave <- unique(subdat[, c(id, wave), drop = FALSE])
  span    <- .weasel_seq_int(lower, upper)

  per_wave <- data.frame(
    wave  = span,
    n_ids = as.integer(table(factor(as.integer(id_wave[[wave]]),
                                    levels = span))),
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

  sentence <- paste0(
    "Scenario ", dQuote(scenario, FALSE), " selects ", headline$n_ids,
    " respondent(s) and yields ", headline$n_rows,
    " row(s) in long format for waves ", lower, " to ", upper,
    " (L = ", L, "). ",
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
