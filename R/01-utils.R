# internal utility helpers and package state
# none of these are exported; helpers are prefixed with .weasel_

# package-internal state container (replaces the former global weasel_env)
the <- new.env(parent = emptyenv())

.weasel_stop <- function(...) stop(paste0(...), call. = FALSE)

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

# always raises a real warning condition so behaviour does not depend on
# which optional packages are installed
.weasel_warn <- function(...) warning(paste0(...), call. = FALSE)

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

# integer sequence that never counts downwards: a > b yields integer(0)
.weasel_seq_int <- function(a, b) {
  a <- as.integer(a)
  b <- as.integer(b)
  if (is.na(a) || is.na(b) || a > b) integer(0) else seq.int(a, b)
}

.weasel_unique_int <- function(x) sort(unique(as.integer(x[!is.na(x)])))

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
  if (!(id %in% names(data))) .weasel_stop("id column not found: ", id)
  if (!(wave %in% names(data))) .weasel_stop("wave column not found: ", wave)
  if (is.list(data[[id]])) .weasel_stop("id column must be an atomic vector.")
  invisible(.weasel_check_wave(data[[wave]], wave))
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
  for (nm in c("max_missing", "n_gap_max", "max_gap_max")) {
    s[[nm]] <- suppressWarnings(as.numeric(s[[nm]]))
    if (anyNA(s[[nm]]) || anyNA(s$require_endpoints)) {
      .weasel_stop("scenario column '", nm,
                   "' (and require_endpoints) must not contain NA.")
    }
    if (any(s[[nm]] < 0)) .weasel_stop("scenario column '", nm, "' must be >= 0.")
  }
  s
}

# run-length gap metrics over the full presence vector
# a gap is any maximal run of FALSE, including leading/trailing runs
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
