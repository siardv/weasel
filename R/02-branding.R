# console branding helpers
# only weasel_logo() (and its deprecated alias) is exported; everything
# else is internal cosmetics

.weasel_palette <- c("green", "red", "yellow", "blue", "magenta", "cyan")

# evaluate expr under the package's private cosmetic rng stream: the
# caller's .Random.seed is saved and restored exactly, while the
# private stream persists in the package state and advances across
# calls. a plain save/restore would replay the caller's state on every
# draw and freeze the colours; a persistent private stream keeps them
# rotating without ever touching the caller's sequence
#' @noRd
.weasel_with_cosmetic_seed <- function(expr) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      the$cosmetic_seed <- get(".Random.seed", envir = globalenv(),
                               inherits = FALSE)
    }
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  if (!is.null(the$cosmetic_seed)) {
    assign(".Random.seed", the$cosmetic_seed, envir = globalenv())
  } else {
    # first cosmetic draw of the session: seed the private stream from
    # the wall clock (millisecond resolution) and the process id
    RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
            sample.kind = "Rejection")
    seed_val <- as.numeric(Sys.time()) * 1000 + Sys.getpid()
    set.seed(as.integer(seed_val %% .Machine$integer.max))
  }
  expr
}

# random colour ordering with the colours in ... never first; drawn
# from the private cosmetic stream so repeated calls keep rotating and
# the caller's RNG stream is never perturbed
#' @noRd
sample_colors <- function(...) {
  excluded <- as.character(c(...))
  .weasel_with_cosmetic_seed({
    head_pool <- setdiff(.weasel_palette, excluded)
    if (length(head_pool) == 0) head_pool <- .weasel_palette
    first <- sample(head_pool, 1)
    rest <- setdiff(.weasel_palette, first)
    c(first, sample(rest))
  })
}

# apply a cli colour if the console supports ansi colours, otherwise
# return the string unchanged
#' @noRd
str_colorize <- function(str, color = NULL) {
  if (is.null(color) || !.weasel_cli()) return(str)
  if (cli::num_ansi_colors() <= 1) return(str)
  f <- tryCatch(
    utils::getFromNamespace(paste0("col_", color), ns = "cli"),
    error = function(e) NULL
  )
  if (is.null(f)) return(str)
  as.character(f(str))[1]
}

# render layered text, optionally splitting layers on ";" and colouring
# each fragment; returns the string when save = TRUE, otherwise prints
#' @noRd
colorize_output <- function(l, ascii = FALSE, version = FALSE,
                            save = FALSE, not_first = "green") {
  layers <- lapply(l, function(z) {
    if (ascii) strsplit(z, ";", fixed = TRUE)[[1]] else z
  })
  colors <- sample_colors(not_first)
  lines <- vapply(layers, function(pieces) {
    cols <- rep_len(colors, length(pieces))
    paste0(mapply(str_colorize, pieces, cols, USE.NAMES = FALSE),
           collapse = "")
  }, character(1))
  out <- paste0(lines, collapse = "\n")
  if (version) {
    v <- tryCatch(as.character(utils::packageVersion("weasel")),
                  error = function(e) "dev")
    out <- paste0(out, "\n\n ", str_colorize(paste0("(v", v, ")"), "silver"), "\n")
  }
  if (save) return(out)
  cat(out, "\n", sep = "")
  invisible(NULL)
}

# styled "WEASEL" label used in status messages
#' @noRd
weasel_text <- function(pre = NULL, post = NULL, not_first = "green") {
  x <- colorize_output(strsplit("WEASEL", split = "")[1],
                       save = TRUE, not_first = not_first)
  paste0(pre, x, post)
}

#' Display the weasel ASCII logo
#'
#' Prints a coloured ASCII art logo to the console. Colour ordering is
#' randomised cosmetically without affecting the caller's RNG state.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' weasel_logo()
#'
#' @export
weasel_logo <- function() {
  layers <- list(
    " ._      __ ;.___ ;.__ ;  .___;  .___ ;._",
    " | | /| / /;/___/;/ _ |; / __/; /___/;/ /",
    " | |/ |/ /;/__/ ;/ __ |; _\\ \\; /__/; / /__,",
    " |__/|__/;/___/;/_/ |_|;/___/;/___/;/____/ "
  )
  colorize_output(layers, ascii = TRUE, version = TRUE, save = FALSE)
}

#' @rdname weasel-deprecated
#' @export
logo <- function(...) {
  .weasel_deprecate("logo", "weasel_logo")
  weasel_logo(...)
}
