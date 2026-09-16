# verify the maintained path policy from the repository root
# run separately from package tests: these policies are not shipped in the tarball

fail <- function(label, paths) {
  stop(paste0(label, ":\n", paste(paths, collapse = "\n")), call. = FALSE)
}

if (!all(file.exists(c(".gitignore", ".Rbuildignore", "DESCRIPTION")))) {
  stop("run from the repository root", call. = FALSE)
}
if (!nzchar(Sys.which("git"))) stop("git is required", call. = FALSE)

tracked <- system2("git", "ls-files", stdout = TRUE)
if (!is.null(attr(tracked, "status")) || !length(tracked)) {
  stop("could not list tracked repository files", call. = FALSE)
}
package_tracked <- tracked[grepl(
  "^(DESCRIPTION|NAMESPACE|LICENSE|NEWS[.]md)$|^(R|man|tests|vignettes|inst)/",
  tracked
)]
repository_tracked <- setdiff(tracked, package_tracked)

# source categories allow normal additions without listing every filename
package_allowed <- c(
  package_tracked, "R/new_source.R", "man/new_function.Rd",
  "tests/testthat/test-new-feature.R", "tests/testthat/helper-new.R",
  "vignettes/new-vignette.Rmd"
)
generated_allowed <- c(
  "build/vignette.rds", "inst/doc/introduction.R", "inst/doc/introduction.Rmd",
  "inst/doc/introduction.html", "inst/doc/new-vignette.R",
  "inst/doc/new-vignette.Rmd", "inst/doc/new-vignette.html"
)
repository_allowed <- c(repository_tracked, "tools/check_inclusion_policy.R",
                         ".github/workflows/new-check.yaml")
# derive out-of-policy probes from maintained paths, without a denylist
probe_paths <- unique(c(package_allowed, generated_allowed, repository_allowed))
rejected <- unique(c(
  paste0(probe_paths, ".extra"),
  file.path(dirname(probe_paths), paste0(".", basename(probe_paths))),
  file.path(dirname(probe_paths), "unlisted", basename(probe_paths))
))
# fixed assets require an explicit allowance for another file of the same type
fixed_paths <- c(
  "README.md", "tests/testthat/_snaps/snapshots.md", "docs/index.html",
  "tools/check_inclusion_policy.R", "inst/examples/example_usage.R",
  "inst/CITATION", "build/vignette.rds"
)
rejected <- unique(c(
  rejected, file.path(dirname(fixed_paths), paste0("unlisted_", basename(fixed_paths)))
))
# normalize root-level probes to the form returned by git check-ignore
rejected <- sub("^[.]/", "", rejected)

# use R's actual filter, including its built-in exclusions and case handling
build_allowed <- unique(c(package_allowed, generated_allowed))
ancestors <- unique(unlist(lapply(build_allowed, function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) return(character())
  vapply(seq_len(length(parts) - 1L), function(i) {
    paste(parts[seq_len(i)], collapse = "/")
  }, character(1))
})))
build_allowed <- c(build_allowed, ancestors)
lost <- build_allowed[tools:::inRbuildignore(build_allowed, ".")]
if (length(lost)) fail("required package paths excluded", lost)
build_rejected <- unique(c(repository_allowed, rejected))
leaked <- build_rejected[!tools:::inRbuildignore(build_rejected, ".")]
if (length(leaked)) fail("unnecessary package paths admitted", leaked)

# an isolated repository proves the shared policy without local/global excludes
check_git <- function() {
  probe_dir <- tempfile("weasel_inclusion_")
  dir.create(probe_dir)
  on.exit(unlink(probe_dir, recursive = TRUE), add = TRUE)
  git_args <- c("-C", shQuote(probe_dir), "-c", "core.excludesFile=")
  status <- system2("git", c(git_args, "init", "--quiet", "--template="))
  if (status != 0L) stop("could not initialize policy probe", call. = FALSE)
  file.copy(".gitignore", file.path(probe_dir, ".gitignore"))
  allowed <- unique(c(package_allowed, repository_allowed))
  ignored <- unique(c(generated_allowed, rejected))
  paths <- c(allowed, ignored)
  for (parent in unique(dirname(paths))) {
    dir.create(file.path(probe_dir, parent), recursive = TRUE, showWarnings = FALSE)
  }
  input <- file.path(probe_dir, "paths.txt")
  # binary output prevents Windows from translating LF records to CRLF
  writeBin(charToRaw(paste0(paths, "\n", collapse = "")), input)
  # guard the input protocol before Git interprets any CR as part of a filename
  input_bytes <- readBin(input, "raw", n = file.info(input)$size)
  if (any(input_bytes == as.raw(13L)) ||
      sum(input_bytes == as.raw(10L)) != length(paths)) {
    stop("git path input must use one LF per path and no CR", call. = FALSE)
  }
  actual <- suppressWarnings(system2(
    "git", c(git_args, "check-ignore", "--no-index", "--stdin"),
    stdin = input, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(actual, "status")
  if (!is.null(status) && status != 1L) fail("git check-ignore failed", actual)
  lost <- intersect(allowed, actual)
  if (length(lost)) fail("maintained repository paths ignored", lost)
  leaked <- setdiff(ignored, actual)
  if (length(leaked)) fail("unnecessary repository paths admitted", leaked)
}
check_git()

cat("inclusion policy OK:", length(tracked), "tracked files,",
    length(build_allowed), "allowed package paths,", length(rejected),
    "rejected examples; generated docs stay out of Git\n")
