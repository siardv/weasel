# methodological justification generator

#' Generate a justification paragraph for a chosen scenario
#'
#' Produces a plain-text paragraph that researchers can paste into a
#' methods section to explain and justify their subset selection. The
#' text references the structural constraints (endpoints, interior
#' gaps, span length), frames the choice as a coverage-vs-sample-size
#' trade-off, and, unless disabled, appends an in-text citation whose
#' author and year are derived automatically from the package's own
#' citation metadata (`utils::citation()`). A scenario that retains no
#' respondents cannot be justified and raises a classed error
#' (`weasel_error_empty_scenario`).
#'
#' @param plan_obj Object returned by [weasel_plan()].
#' @param scenario Name (or unambiguous abbreviation) of the scenario.
#' @param style One of `"methods"` (full methods-section paragraph),
#'   `"concise"` (short summary), or `"extended"` (detailed
#'   rationale).
#' @param cite If `TRUE`, append an in-text citation. The author and
#'   year are filled in automatically from the package's citation
#'   metadata; a plain `(R package <name>)` reference is used only
#'   when that metadata is unavailable.
#' @param author Optional author string overriding the automatically
#'   derived citation author.
#' @param year Optional year string overriding the automatically
#'   derived citation year.
#' @param package Package whose citation metadata is used, and the
#'   name shown in the fallback reference.
#' @param acronym Acronym for the framework.
#' @param full_name Full name of the framework.
#' @param digits Number of decimal places for numeric values.
#'
#' @return A single character string ready to paste into a manuscript.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 200, n_times = 10, seed = 1)
#' p <- weasel_plan(d, "id", "time", span = "core")
#'
#' # the in-text citation (package author and year) is automatic
#' cat(weasel_justify_subset(p, "anchored_balanced"), "\n")
#' cat(weasel_justify_subset(p, "anchored_strict", style = "concise"), "\n")
#' cat(weasel_justify_subset(p, "anchored_balanced", style = "extended"), "\n")
#'
#' # override the derived citation, or omit it entirely
#' cat(weasel_justify_subset(p, "anchored_balanced",
#'                           author = "Custom Name", year = "2030"), "\n")
#' cat(weasel_justify_subset(p, "anchored_balanced", cite = FALSE), "\n")
#'
#' @export
weasel_justify_subset <- function(plan_obj,
                                  scenario = "anchored_balanced",
                                  style = c("methods", "concise", "extended"),
                                  cite = TRUE,
                                  author = NULL,
                                  year = NULL,
                                  package = "weasel",
                                  acronym = "WEASEL",
                                  full_name = "Wave-based Extraction and Selection for Longitudinal Data",
                                  digits = 3) {
  style <- match.arg(style)
  .weasel_check_plan(plan_obj)

  scenario <- weasel_match_scenario(scenario, plan_obj$plan$scenario)
  row <- plan_obj$plan[plan_obj$plan$scenario == scenario, , drop = FALSE]
  if (nrow(row) != 1) {
    .weasel_stop("scenario not found or ambiguous.",
                 class = "weasel_error_scenario")
  }
  n_check <- suppressWarnings(as.integer(row$n_ids[[1]]))
  if (!is.na(n_check) && n_check == 0) {
    .weasel_stop("scenario '", scenario, "' retains no respondents; ",
                 "there is nothing to justify.",
                 class = "weasel_error_empty_scenario")
  }

  get1 <- function(x) if (length(x) == 0) NA else x[[1]]

  lower             <- as.integer(get1(row$lower))
  upper             <- as.integer(get1(row$upper))
  L                 <- as.integer(get1(row$L))
  n_ids             <- as.integer(get1(row$n_ids))
  require_endpoints <- as.logical(get1(row$require_endpoints))
  max_missing       <- as.integer(get1(row$max_missing))
  n_gap_max         <- as.integer(get1(row$n_gap_max))
  # plans saved before 0.4.0 store this column as max_gap_max
  gl_col      <- if (!is.null(row[["max_gap_len"]])) {
    row[["max_gap_len"]]
  } else {
    row[["max_gap_max"]]
  }
  max_gap_len <- as.integer(get1(gl_col))
  mean_prop_present <- suppressWarnings(as.numeric(get1(row$mean_prop_present)))
  endpoint_rate     <- suppressWarnings(as.numeric(get1(row$endpoint_rate)))
  span_reason <- if ("span_reason" %in% names(row)) {
    as.character(get1(row$span_reason))
  } else {
    NA_character_
  }
  note <- if ("note" %in% names(row)) as.character(get1(row$note)) else NA_character_
  grid <- .weasel_or(plan_obj$grid, "consecutive")

  pop <- plan_obj[["population"]]
  pop_txt <- if (!is.null(pop)) {
    sprintf(
      "The planning population comprised %s respondent(s) observed at least once within the analysis window, out of %s distinct respondent(s) in the supplied data; retention figures are relative to this in-window population.",
      pop$n_ids_in_span, pop$n_ids_data
    )
  } else {
    NULL
  }

  fmt <- function(x) .weasel_format_num(x, digits)

  waves_txt <- if (!is.na(lower) && !is.na(upper)) {
    sprintf("waves %s to %s", lower, upper)
  } else {
    "the selected wave window"
  }
  L_txt <- if (!is.na(L)) {
    if (identical(grid, "observed")) {
      sprintf("L = %s observed waves", L)
    } else {
      sprintf("L = %s", L)
    }
  } else {
    NULL
  }

  endpoint_txt <- if (isTRUE(require_endpoints)) {
    "required observed endpoints to ensure temporal anchoring"
  } else {
    "did not require observed endpoints, allowing unanchored participation"
  }

  miss_txt <- if (!is.na(max_missing)) {
    if (max_missing == 0) {
      "required complete participation within the window (no missing waves)"
    } else if (max_missing == 1) {
      "allowed at most one missing wave within the window"
    } else {
      sprintf("allowed up to %s missing waves within the window", max_missing)
    }
  } else {
    "applied an explicit limit on missing waves within the window"
  }

  gap_txt <- NULL
  if (!is.na(n_gap_max) && !is.na(max_gap_len)) {
    gap_txt <- sprintf(
      "and restricted the missingness structure (at most %s interior missing block(s), each no longer than %s wave(s))",
      n_gap_max, max_gap_len
    )
  }

  cov_txt <- if (!is.na(mean_prop_present)) {
    sprintf("mean within-window coverage was %s", fmt(mean_prop_present))
  } else {
    NULL
  }
  end_rate_txt <- if (!is.na(endpoint_rate)) {
    sprintf("endpoint coverage was %s", fmt(endpoint_rate))
  } else {
    NULL
  }
  diag_parts <- c(cov_txt, end_rate_txt)

  cite_txt <- ""
  if (isTRUE(cite)) {
    # fill in whatever the caller did not supply from the package's own
    # citation metadata, so the reference is automatic by default
    if (is.null(author) || is.null(year)) {
      meta <- .weasel_citation_meta(package)
      if (is.null(author)) author <- meta$author
      if (is.null(year))   year   <- meta$year
    }
    if (!is.null(author) && !is.null(year)) {
      cite_txt <- sprintf(" (%s, %s)", author, year)
    } else if (!is.null(author)) {
      cite_txt <- sprintf(" (%s)", author)
    } else {
      cite_txt <- sprintf(" (R package %s)", package)
    }
  }

  if (style == "concise") {
    parts <- c(
      sprintf(
        "We selected a longitudinal analysis subset using the %s framework (%s)%s.",
        acronym, full_name, cite_txt
      ),
      sprintf(
        "Within %s, we %s, %s%s.",
        waves_txt, endpoint_txt, miss_txt,
        if (!is.null(gap_txt)) paste0(" ", gap_txt) else ""
      ),
      if (!is.na(n_ids)) {
        sprintf("This strategy retained %s respondent(s).", n_ids)
      } else NULL,
      if (length(diag_parts) > 0) {
        paste0("In the resulting subset, ",
               paste(diag_parts, collapse = "; "), ".")
      } else NULL
    )
    return(.collapse_parts(parts))
  }

  if (style == "methods") {
    parts <- c(
      sprintf(
        "To construct a longitudinal analysis sample, we selected respondents whose wave participation satisfied explicit structural criteria using the %s framework (%s)%s.",
        acronym, full_name, cite_txt
      ),
      sprintf(
        "Specifically, we focused on %s%s and %s, %s%s.",
        waves_txt,
        if (!is.null(L_txt)) sprintf(" (%s)", L_txt) else "",
        endpoint_txt,
        miss_txt,
        if (!is.null(gap_txt)) paste0(", ", gap_txt) else ""
      ),
      if (!is.na(n_ids)) {
        sprintf(
          "This strategy retained %s respondent(s), reflecting an explicit trade-off between sample size and within-window completeness.",
          n_ids
        )
      } else {
        "This strategy reflects an explicit trade-off between sample size and within-window completeness."
      },
      pop_txt,
      if (length(diag_parts) > 0) {
        paste0("In the resulting subset, ",
               paste(diag_parts, collapse = ", "), ".")
      } else NULL,
      if (!is.na(note) && nzchar(note)) {
        sprintf("This scenario is characterized as: %s.", note)
      } else NULL,
      if (!is.na(span_reason) && nzchar(span_reason)) {
        if (identical(span_reason, "explicit")) {
          "The analysis window was fixed a priori through explicit bounds supplied to the planning call, rather than chosen by an automatic rule."
        } else {
          sprintf(
            "The analysis window was selected using the package's span rule (%s), which prioritizes a coherent window with comparatively strong participation.",
            span_reason
          )
        }
      } else NULL,
      "All selection decisions were rule-based and reproducible, and can be regenerated from the same inputs and parameters using the weasel workflow."
    )
    return(.collapse_parts(parts))
  }

  parts <- c(
    sprintf(
      "We used the %s framework (%s)%s to derive a reproducible longitudinal analysis subset from the available panel waves.",
      acronym, full_name, cite_txt
    ),
    "The goal was to avoid ad hoc inclusion rules by explicitly defining admissible participation patterns and selecting a subset that balances longitudinal completeness with sample size.",
    sprintf(
      "We defined a target window of %s%s and applied structural constraints to respondent trajectories.",
      waves_txt, if (!is.null(L_txt)) sprintf(" (%s)", L_txt) else ""
    ),
    sprintf(
      "Within this window, we %s, %s%s.",
      endpoint_txt, miss_txt,
      if (!is.null(gap_txt)) paste0(", ", gap_txt) else ""
    ),
    if (!is.na(n_ids)) {
      sprintf("Under these criteria, %s respondent(s) were retained.", n_ids)
    } else NULL,
    pop_txt,
    if (length(diag_parts) > 0) {
      paste0("Subset diagnostics indicated that ",
             paste(diag_parts, collapse = " and "), ".")
    } else NULL,
    if (!is.na(note) && nzchar(note)) {
      sprintf(
        "Relative to alternative scenarios considered by the package, this configuration is described as: %s.",
        note
      )
    } else NULL,
    if (!is.na(span_reason) && nzchar(span_reason)) {
      if (identical(span_reason, "explicit")) {
        "The analysis window was fixed a priori through explicit bounds supplied to the planning call, rather than produced by a coverage rule; the window choice is therefore a design decision documented here."
      } else {
        sprintf(
          "The chosen window was produced by the package's span rule (%s). In practice, this emphasizes a stable segment of the panel rather than maximizing the nominal wave range.",
          span_reason
        )
      }
    } else NULL,
    "This approach improves transparency because the inclusion set is fully determined by declared constraints (window bounds, endpoint handling, and permitted missingness structure) rather than subjective post hoc decisions."
  )
  .collapse_parts(parts)
}

# drop NAs and empty strings, then paste; c() already removes NULLs
#' @noRd
.collapse_parts <- function(parts) {
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = " ")
}

# in-text citation author and year from the package's own citation
# metadata; NULL fields when the metadata is unavailable, in which case
# the caller falls back to the plain package reference
#' @noRd
.weasel_citation_meta <- function(package) {
  out <- list(author = NULL, year = NULL)
  entry <- tryCatch(
    unclass(utils::citation(package))[[1L]],
    error = function(e) NULL
  )
  if (is.null(entry)) return(out)
  fam <- tryCatch(
    vapply(entry$author,
           function(p) paste(p$family, collapse = " "), character(1)),
    error = function(e) character(0)
  )
  fam <- fam[!is.na(fam) & nzchar(fam)]
  if (length(fam) == 1) {
    out$author <- fam
  } else if (length(fam) == 2) {
    out$author <- paste(fam, collapse = " & ")
  } else if (length(fam) > 2) {
    out$author <- paste0(fam[1L], " et al.")
  }
  yr <- tryCatch(as.character(entry$year), error = function(e) NULL)
  if (length(yr) == 1 && !is.na(yr) && nzchar(yr)) out$year <- yr
  out
}
