# selection diagnostics: tolerance sensitivity and attrition selectivity

#' Sweep selection tolerances and count retained respondents
#'
#' Evaluates every combination of the supplied tolerance values against
#' the per-respondent metrics stored in a plan, reporting how many
#' respondents each combination would retain. Because the sweep reuses
#' the metrics that [weasel_plan()] already computed, it is fast even
#' for large panels and works when the plan was created with
#' `keep_data = FALSE`. Use it to show how sensitive the sample size is
#' to the chosen tolerances before committing to a scenario.
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param require_endpoints Logical values to evaluate.
#' @param max_missing Integer tolerances for missing waves in the span.
#' @param n_gap_max Integer tolerances for the number of interior gaps.
#' @param max_gap_max Integer tolerances for the longest interior gap.
#'
#' @return A data frame with one row per combination and the columns
#'   `require_endpoints`, `max_missing`, `n_gap_max`, `max_gap_max`,
#'   `n_ids`, `prop_ids` (share of all respondents observed in the
#'   span), and `mean_prop_present` (coverage among the retained
#'   respondents; `NA` when nobody is retained).
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 150, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' sens <- weasel_sensitivity(p, max_missing = 0:2)
#' weasel_print_table(sens, title = "Tolerance sweep", n = 12)
#'
#' @export
weasel_sensitivity <- function(plan_obj,
                               require_endpoints = c(TRUE, FALSE),
                               max_missing = 0:3,
                               n_gap_max = 0:2,
                               max_gap_max = 0:2) {
  .weasel_check_plan(plan_obj)
  idm <- plan_obj$id_metrics
  if (!is.data.frame(idm) || nrow(idm) == 0) {
    .weasel_stop("plan_obj$id_metrics is missing or empty.")
  }

  require_endpoints <- unique(as.logical(require_endpoints))
  if (length(require_endpoints) == 0 || anyNA(require_endpoints)) {
    .weasel_stop("require_endpoints must contain TRUE and/or FALSE.")
  }
  check_tol <- function(x, name) {
    ok <- is.numeric(x) && length(x) > 0 && !anyNA(x) &&
      all(is.finite(x)) && all(abs(x - round(x)) <= 1e-8) && all(x >= 0)
    if (!ok) {
      .weasel_stop(name, " must be non-negative integers ",
                   "(fractional values are rejected, not truncated).")
    }
    sort(unique(as.integer(round(x))))
  }
  max_missing <- check_tol(max_missing, "max_missing")
  n_gap_max   <- check_tol(n_gap_max, "n_gap_max")
  max_gap_max <- check_tol(max_gap_max, "max_gap_max")

  g <- expand.grid(
    max_gap_max       = max_gap_max,
    n_gap_max         = n_gap_max,
    max_missing       = max_missing,
    require_endpoints = require_endpoints,
    KEEP.OUT.ATTRS    = FALSE
  )
  g <- g[c("require_endpoints", "max_missing", "n_gap_max", "max_gap_max")]
  g <- g[order(!g$require_endpoints, g$max_missing, g$n_gap_max,
               g$max_gap_max), , drop = FALSE]
  rownames(g) <- NULL

  n_total  <- nrow(idm)
  anchored <- idm$has_lower & idm$has_upper
  res <- vapply(seq_len(nrow(g)), function(i) {
    keep <- idm$n_missing <= g$max_missing[i] &
      idm$n_gap <= g$n_gap_max[i] &
      idm$max_gap <= g$max_gap_max[i]
    if (g$require_endpoints[i]) keep <- keep & anchored
    c(
      n   = sum(keep),
      cov = if (any(keep)) mean(idm$prop_present[keep]) else NA_real_
    )
  }, numeric(2))

  g$n_ids             <- as.integer(res["n", ])
  g$prop_ids          <- g$n_ids / n_total
  g$mean_prop_present <- res["cov", ]
  g
}

#' Compare retained and excluded respondents on covariates
#'
#' Selecting respondents by participation completeness can bias a sample
#' when retention is related to substantive characteristics. This
#' diagnostic compares the respondents a scenario retains with those who
#' are observed in the span but excluded, covariate by covariate, and
#' reports the standardized mean difference (SMD) for each.
#'
#' For every respondent, a single value per covariate is taken either
#' from their first observed wave inside the span (`at = "first"`, the
#' usual baseline comparison) or as the mean over their observed waves
#' in the span (`at = "mean"`). Missing item values are dropped within
#' each group. Duplicated (id, wave) rows trigger a classed warning
#' (`weasel_duplicates`) and their covariate values are averaged within
#' each pair first, so the diagnostic counts each pair once and does
#' not depend on the row order of the input. The SMD divides the group difference by the pooled
#' standard deviation `sqrt((sd_retained^2 + sd_excluded^2) / 2)`;
#' absolute values around 0.1 or larger are commonly read as noteworthy
#' imbalance. The SMD is `NA` when either group has no spread.
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param scenario Name (or unambiguous abbreviation) of the scenario.
#' @param vars Character vector of covariate columns to compare.
#'   Defaults to every numeric or logical column except the id and wave
#'   columns.
#' @param data Optional long-format data frame; defaults to the data
#'   attached to `plan_obj`. Required when the plan was created with
#'   `keep_data = FALSE`.
#' @param at Either `"first"` (value at the first observed wave in the
#'   span) or `"mean"` (mean over observed waves in the span).
#'
#' @return A data frame with one row per covariate and the columns
#'   `variable`, `n_retained`, `n_excluded` (respondents with a
#'   non-`NA` value), `mean_retained`, `mean_excluded`, `diff`
#'   (retained minus excluded), and `smd`, sorted by absolute SMD,
#'   largest first.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 200, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#' sel <- weasel_selectivity(p, "anchored_balanced")
#' weasel_print_table(sel, title = "Retained vs excluded", digits = 3)
#'
#' @export
weasel_selectivity <- function(plan_obj, scenario, vars = NULL, data = NULL,
                               at = c("first", "mean")) {
  at <- match.arg(at)
  .weasel_check_plan(plan_obj)

  scenario <- weasel_match_scenario(scenario, plan_obj$plan$scenario)
  row <- plan_obj$plan[plan_obj$plan$scenario == scenario, , drop = FALSE]
  if (nrow(row) != 1) {
    .weasel_stop("scenario not found or ambiguous.",
                 class = "weasel_error_scenario")
  }

  if (is.null(data)) data <- attr(plan_obj, "data")
  if (is.null(data)) {
    .weasel_stop("no data available: the plan was created with ",
                 "keep_data = FALSE; pass the original data via the ",
                 "'data' argument.")
  }
  id   <- plan_obj$id
  wave <- plan_obj$wave
  .weasel_check_id_wave(data, id, wave)

  ids_keep <- row$ids[[1]]
  if (length(ids_keep) == 0) {
    .weasel_stop("scenario '", scenario, "' retains no respondents.")
  }
  sp <- .weasel_plan_span(plan_obj, row)

  all_ids  <- plan_obj$id_metrics[[id]]
  excluded <- setdiff(all_ids, ids_keep)
  if (length(excluded) == 0) {
    .weasel_stop("scenario '", scenario, "' retains every respondent in ",
                 "the span; there is no excluded group to compare against.")
  }

  numish <- vapply(data, function(z) is.numeric(z) || is.logical(z),
                   logical(1))
  if (is.null(vars)) {
    vars <- setdiff(names(data)[numish], c(id, wave))
  } else {
    vars <- as.character(vars)
    missing_v <- setdiff(vars, names(data))
    if (length(missing_v) > 0) {
      .weasel_stop("vars not found in data: ",
                   paste(missing_v, collapse = ", "))
    }
    bad <- vars[!numish[vars]]
    if (length(bad) > 0) {
      .weasel_stop("vars must be numeric or logical columns; not: ",
                   paste(bad, collapse = ", "))
    }
    vars <- setdiff(vars, c(id, wave))
  }
  if (length(vars) == 0) .weasel_stop("no numeric covariates to compare.")

  w_int <- as.integer(round(data[[wave]]))
  sub <- data[!is.na(data[[id]]) & (w_int %in% sp), , drop = FALSE]

  # duplicated (id, wave) rows would make the baseline depend on the
  # input row order (at = "first") or be double-counted (at = "mean");
  # average covariates within each duplicated pair so the diagnostic is
  # deterministic and counts each pair once, mirroring the plan metrics
  sub <- sub[c(id, wave, vars)]
  w_key <- as.integer(round(sub[[wave]]))
  dd <- .weasel_dedup_index(sub[[id]], w_key)
  if (dd$n_dup > 0) {
    .weasel_warn(
      dd$n_dup, " duplicated (", id, ", ", wave, ") row(s) in the span; ",
      "covariate values are averaged within each duplicated pair so the ",
      "diagnostic is deterministic and counts each pair once.",
      class = "weasel_duplicates"
    )
    o     <- order(sub[[id]], w_key)
    ids_o <- sub[[id]][o]
    w_o   <- w_key[o]
    n_o   <- length(o)
    new_pair <- c(TRUE, ids_o[-1L] != ids_o[-n_o] | w_o[-1L] != w_o[-n_o])
    grp   <- cumsum(new_pair)
    agg <- data.frame(ids_o[new_pair], w_o[new_pair],
                      stringsAsFactors = FALSE)
    names(agg) <- c(id, wave)
    for (v in vars) {
      x    <- as.numeric(sub[[v]][o])
      sums <- rowsum(ifelse(is.na(x), 0, x), grp)
      cnts <- rowsum(as.numeric(!is.na(x)), grp)
      m    <- as.numeric(sums / cnts)
      m[cnts == 0] <- NA_real_
      agg[[v]] <- m
    }
    sub <- agg
  }

  # one baseline value per respondent per covariate
  if (at == "first") {
    o     <- order(sub[[id]], as.integer(round(sub[[wave]])))
    sub_o <- sub[o, , drop = FALSE]
    base  <- sub_o[!duplicated(sub_o[[id]]), c(id, vars), drop = FALSE]
  } else {
    uid <- sort(unique(sub[[id]]))
    gi  <- match(sub[[id]], uid)
    base <- data.frame(uid, stringsAsFactors = FALSE)
    names(base) <- id
    for (v in vars) {
      x <- as.numeric(sub[[v]])
      m <- vapply(split(x, gi), function(z) {
        z <- z[!is.na(z)]
        if (length(z) == 0) NA_real_ else mean(z)
      }, numeric(1))
      base[[v]] <- unname(m)
    }
  }

  retained_flag <- base[[id]] %in% ids_keep

  smd_row <- function(v) {
    x1 <- as.numeric(base[[v]][retained_flag])
    x0 <- as.numeric(base[[v]][!retained_flag])
    x1 <- x1[!is.na(x1)]
    x0 <- x0[!is.na(x0)]
    m1 <- if (length(x1) > 0) mean(x1) else NA_real_
    m0 <- if (length(x0) > 0) mean(x0) else NA_real_
    s1 <- if (length(x1) > 1) stats::sd(x1) else NA_real_
    s0 <- if (length(x0) > 1) stats::sd(x0) else NA_real_
    pooled <- sqrt((s1^2 + s0^2) / 2)
    smd <- if (is.na(pooled) || pooled == 0) NA_real_ else (m1 - m0) / pooled
    data.frame(
      variable = v,
      n_retained = length(x1), n_excluded = length(x0),
      mean_retained = m1, mean_excluded = m0,
      diff = m1 - m0, smd = smd,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, lapply(vars, smd_row))
  out <- out[order(-abs(out$smd), out$variable), , drop = FALSE]
  rownames(out) <- NULL
  out
}
