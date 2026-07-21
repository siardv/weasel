#' Generate synthetic longitudinal panel data
#'
#' Creates a long-format data frame in which wave-level missingness is
#' represented the way real panel files represent it: a respondent who
#' misses a wave simply has no row for that wave. Four participation
#' mechanisms are layered (random skips, attention decay, attrition,
#' and block dropout), and a small amount of item-level missingness
#' (`NA` values inside the outcome columns of otherwise observed rows)
#' can be added on top.
#'
#' The returned grid is therefore deliberately incomplete; this is what
#' both weasel pipelines analyse. The function has no net effect on the
#' caller's RNG state: the state present before the call (including the
#' `RNGkind()` setting) is restored on exit, and reproducibility is
#' controlled through `seed`. Internally the generator pins the RNG
#' configuration to `RNGkind("Mersenne-Twister", "Inversion",
#' sample.kind = "Rejection")`, the R >= 3.6 defaults, so a given seed
#' reproduces the same panel even when the caller uses a non-default
#' sampler (for example `sample.kind = "Rounding"`).
#'
#' @param n_ids Number of respondents.
#' @param n_times Number of time points (waves). Ignored when `waves`
#'   is supplied.
#' @param n_vars Number of outcome variables to generate.
#' @param prop_random Probability that any given (respondent, wave)
#'   observation is skipped at random. Like every `prop_*` parameter,
#'   it must be a single number in `[0, 1]`; invalid values fail
#'   immediately, before any random draw.
#' @param prop_attention Asymptotic probability of attention-related
#'   wave skipping at late waves.
#' @param attention_center Wave position at which the attention-decay
#'   curve is centred. Must be a single finite number.
#' @param attention_scale Steepness of the attention-decay curve;
#'   larger values give a more gradual increase. Must be a single
#'   finite number greater than zero.
#' @param prop_attrition Proportion of respondents who permanently drop
#'   out from a random wave onwards.
#' @param prop_block Proportion of respondents who miss one contiguous
#'   block of waves.
#' @param block_duration_range Integer vector of length 2 giving the
#'   min/max duration of block missingness.
#' @param prop_item_missing Probability that an individual outcome value
#'   on an observed row is `NA` (item nonresponse).
#' @param id_start Starting integer for respondent identifiers.
#' @param waves Optional integer vector of wave labels, for example
#'   `seq(2008, 2032, by = 2)` for a biennial schedule. When supplied it
#'   overrides `n_times`; the participation mechanisms operate on the
#'   positions of this schedule, and the returned `time` column contains
#'   these labels. Analyse such data with `grid = "observed"` in
#'   [weasel_plan()] or [set_weasel_scope()].
#' @param seed Random seed; a single integer-valued number (fractional
#'   and non-numeric values are rejected before `set.seed()` is
#'   reached). If `NULL`, a seed is drawn and reported so the data set
#'   can be regenerated. A given seed reproduces the same panel
#'   regardless of the caller's `RNGkind()`; see Details.
#'
#' @return A data frame in long format with columns `id`, `time`, and
#'   `var1` ... `varN`. Respondents keep at least one observed wave, so
#'   the number of distinct ids always equals `n_ids`, but
#'   `nrow(result)` is typically smaller than `n_ids * n_times`.
#'
#' @examples
#' d <- generate_weasel_dummy_data(n_ids = 50, n_times = 8, seed = 1)
#'
#' # the grid is incomplete: wave-level missingness is row absence
#' nrow(d) < 50 * 8
#'
#' # scope pipeline starts here
#' set_weasel_scope(d, "id", "time")
#' weasel_clear_scope()
#'
#' # plan pipeline starts here
#' p <- weasel_plan(d, "id", "time", span = "core")
#'
#' # a biennial schedule; analyse with grid = "observed"
#' b <- generate_weasel_dummy_data(n_ids = 40, waves = seq(2008, 2020, 2),
#'                                 seed = 1)
#' pb <- weasel_plan(b, "id", "time", span = "full", grid = "observed")
#'
#' @importFrom stats runif rbinom rnorm
#' @export
generate_weasel_dummy_data <- function(n_ids = 1000,
                                       n_times = 13,
                                       n_vars = 5,
                                       prop_random = 0.05,
                                       prop_attention = 0.08,
                                       attention_center = 10,
                                       attention_scale = 2.5,
                                       prop_attrition = 0.06,
                                       prop_block = 0.04,
                                       block_duration_range = c(2, 4),
                                       prop_item_missing = 0.02,
                                       id_start = 800001,
                                       waves = NULL,
                                       seed = NULL) {
  n_ids    <- .weasel_check_count(n_ids, "n_ids")
  n_times  <- .weasel_check_count(n_times, "n_times")
  n_vars   <- .weasel_check_count(n_vars, "n_vars")
  id_start <- .weasel_check_bound(id_start, "id_start")
  # every probability, shape, and seed parameter is validated here,
  # before any random draw, so invalid inputs fail immediately with the
  # parameter named instead of surfacing later as an unrelated sampling
  # error or a silently degenerate panel
  prop_random       <- .weasel_check_prob(prop_random, "prop_random")
  prop_attention    <- .weasel_check_prob(prop_attention, "prop_attention")
  prop_attrition    <- .weasel_check_prob(prop_attrition, "prop_attrition")
  prop_block        <- .weasel_check_prob(prop_block, "prop_block")
  prop_item_missing <- .weasel_check_prob(prop_item_missing,
                                          "prop_item_missing")
  if (!(length(attention_scale) == 1 && is.numeric(attention_scale) &&
        !is.na(attention_scale) && is.finite(attention_scale) &&
        attention_scale > 0)) {
    .weasel_stop("attention_scale must be a single finite number > 0.")
  }
  if (!(length(attention_center) == 1 && is.numeric(attention_center) &&
        !is.na(attention_center) && is.finite(attention_center))) {
    .weasel_stop("attention_center must be a single finite number.")
  }
  if (!is.null(seed)) seed <- .weasel_check_bound(seed, "seed")
  if (is.null(n_ids) || is.null(n_times) || is.null(n_vars) ||
      is.null(id_start)) {
    .weasel_stop("n_ids, n_times, n_vars, and id_start must not be NULL.")
  }
  if (!is.null(waves)) {
    if (!is.numeric(waves) || length(waves) == 0 || anyNA(waves) ||
        any(!is.finite(waves)) || any(abs(waves - round(waves)) > 1e-8)) {
      .weasel_stop("waves must be integer-valued wave labels ",
                   "(fractional values are rejected, not truncated).")
    }
    waves <- sort(unique(as.integer(round(waves))))
    if (length(waves) <= 2) {
      .weasel_stop("waves must contain more than 2 distinct integer values.")
    }
    n_times <- length(waves)
  }
  if (n_ids <= 0) .weasel_stop("n_ids must be > 0.")
  if (n_times <= 2) .weasel_stop("n_times must be > 2.")
  if (n_vars <= 0) .weasel_stop("n_vars must be > 0.")
  if (length(block_duration_range) != 2 ||
      !is.numeric(block_duration_range) || anyNA(block_duration_range) ||
      any(abs(block_duration_range - round(block_duration_range)) > 1e-8) ||
      any(block_duration_range < 1)) {
    .weasel_stop("block_duration_range must be two integers >= 1.")
  }

  .weasel_with_preserved_seed({
    # pin the sampler so a given seed reproduces the same panel under any
    # caller RNGkind(); the caller's state and kind are restored on exit
    RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
            sample.kind = "Rejection")
    if (is.null(seed)) seed <- sample.int(1e6, 1)
    set.seed(seed)
    .weasel_msg("seed: ", seed)

    n_ids   <- as.integer(n_ids)
    n_times <- as.integer(n_times)
    ids_vec <- seq.int(id_start, id_start + n_ids - 1)

    # participation matrix: TRUE = respondent observed at that wave
    present <- matrix(TRUE, nrow = n_ids, ncol = n_times)

    # 1) random skips
    present[stats::runif(n_ids * n_times) < prop_random] <- FALSE

    # 2) attention decay: skip probability rises along a logistic curve
    t_seq <- seq_len(n_times)
    p_att <- prop_attention /
      (1 + exp(-(t_seq - attention_center) / attention_scale))
    p_att <- pmin(pmax(p_att, 0), 0.95)
    att_miss <- matrix(stats::runif(n_ids * n_times), nrow = n_ids) <
      matrix(p_att, nrow = n_ids, ncol = n_times, byrow = TRUE)
    present[att_miss] <- FALSE

    # 3) attrition: permanent dropout from a random wave onwards
    n_drop <- round(n_ids * prop_attrition)
    if (n_drop > 0) {
      drop_rows  <- sample.int(n_ids, n_drop)
      drop_times <- sample.int(n_times - 1, n_drop, replace = TRUE) + 1L
      for (k in seq_len(n_drop)) {
        present[drop_rows[k], drop_times[k]:n_times] <- FALSE
      }
    }

    # 4) block missingness: one contiguous interior block per affected id
    n_block <- round(n_ids * prop_block)
    if (n_block > 0) {
      blk_rows <- sample.int(n_ids, n_block)
      for (k in seq_len(n_block)) {
        blk_start <- sample.int(max(n_times - 2L, 1L), 1) + 1L
        dur <- sample(seq.int(block_duration_range[1],
                              block_duration_range[2]), 1)
        blk_end <- min(n_times, blk_start + dur - 1L)
        present[blk_rows[k], blk_start:blk_end] <- FALSE
      }
    }

    # guarantee every respondent keeps at least one observed wave
    empty <- which(rowSums(present) == 0L)
    for (r in empty) present[r, sample.int(n_times, 1)] <- TRUE

    obs <- which(t(present))
    data <- data.frame(
      id   = as.integer(ids_vec[(obs - 1L) %/% n_times + 1L]),
      time = as.integer((obs - 1L) %% n_times + 1L),
      stringsAsFactors = FALSE
    )
    # map schedule positions to the supplied wave labels
    if (!is.null(waves)) data$time <- waves[data$time]

    # outcome variables on observed rows only
    var_generators <- list(
      function(n) sample(0:15, n, replace = TRUE),
      function(n) round(stats::runif(n, 5, 30), 1),
      function(n) stats::rbinom(n, 1, 0.3),
      function(n) sample(1:7, n, replace = TRUE),
      function(n) round(stats::rnorm(n, mean = 10, sd = 3), 1)
    )
    n_obs <- nrow(data)
    for (i in seq_len(n_vars)) {
      g <- var_generators[[((i - 1L) %% length(var_generators)) + 1L]]
      v <- g(n_obs)
      if (prop_item_missing > 0) {
        v[stats::runif(n_obs) < prop_item_missing] <- NA
      }
      data[[paste0("var", i)]] <- v
    }

    data[order(data$id, data$time), , drop = FALSE]
  })
}
