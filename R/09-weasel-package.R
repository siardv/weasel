#' weasel: Wave-based Extraction and Selection for Longitudinal Data
#'
#' Tools for selecting, filtering, and balancing longitudinal panel data
#' across survey waves. Provides a scope-based pipeline for wave pattern
#' analysis and a planning interface for scenario-based respondent
#' selection with configurable missingness tolerances, interior-gap
#' constraints, and endpoint requirements. Throughout the package a
#' respondent counts as observed at a wave when a row with that
#' (id, wave) pair exists in the long-format data.
#'
#' @section Core workflows:
#' The package supports two main approaches.
#'
#' \strong{Scope pipeline} (interactive exploration):
#' \code{set_weasel_scope()} then \code{evaluate_weasel_scope()} then
#' \code{weasel_reshape_to_wide()} then \code{weasel_summarize_waves()}
#' then \code{weasel_filter_wave_summary()} or
#' \code{weasel_get_data_by_row()}; inspect the state at any point with
#' \code{weasel_scope_info()} and finish with
#' \code{weasel_clear_scope()}.
#'
#' \strong{Plan pipeline} (scenario comparison):
#' \code{weasel_plan()} then \code{weasel_compare_scenarios()} then
#' \code{weasel_summarize_subset()} or \code{weasel_apply()}. Audit the
#' selection with \code{weasel_sensitivity()} (how the sample size
#' reacts to the tolerances) and \code{weasel_selectivity()} (whether
#' retained and excluded respondents differ on covariates).
#'
#' Use \code{weasel_justify_subset()} to generate a ready-to-paste
#' methods-section paragraph for a chosen scenario.
#'
#' @section Wave grids:
#' Both pipelines evaluate presence over a wave grid. The default,
#' \code{grid = "consecutive"}, treats every integer between the span
#' bounds as a scheduled wave and warns when some of those waves have no
#' observations at all. For biennial and other non-consecutive
#' schedules (for example waves recorded as years), use
#' \code{grid = "observed"} so that only wave values occurring in the
#' data count as scheduled.
#'
#' @section Options:
#' Set \code{options(weasel.verbose = FALSE)} to silence status
#' messages.
#'
#' @importFrom utils head packageVersion
#' @keywords internal
"_PACKAGE"
