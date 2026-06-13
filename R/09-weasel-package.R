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
#' \code{reshape_to_wide()} then \code{summarize_waves()} then
#' \code{filter_wave_summary()} or \code{get_data_by_row()}; finish with
#' \code{weasel_clear_scope()}.
#'
#' \strong{Plan pipeline} (scenario comparison):
#' \code{weasel_plan()} then \code{weasel_compare_scenarios()} then
#' \code{weasel_summarize_subset()} or \code{weasel_apply()}.
#'
#' Use \code{weasel_justify_subset()} to generate a ready-to-paste
#' methods-section paragraph for a chosen scenario.
#'
#' @section Options:
#' Set \code{options(weasel.verbose = FALSE)} to silence status
#' messages.
#'
#' @importFrom utils head packageVersion
#' @keywords internal
"_PACKAGE"
