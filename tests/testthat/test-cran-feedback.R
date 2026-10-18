testthat::test_that("informational displays can be suppressed as messages", {
  old <- options(weasel.verbose = TRUE)
  on.exit(options(old), add = TRUE)
  on.exit(weasel_clear_scope(), add = TRUE)
  weasel_clear_scope()
  testthat::expect_silent(suppressMessages(weasel_logo()))
  testthat::expect_silent(suppressMessages(weasel_scope_info()))
  testthat::expect_silent(suppressMessages(weasel_example(seed = 42)))
})

testthat::test_that("quiet mode keeps demonstration and table return values", {
  old <- options(weasel.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  on.exit(weasel_clear_scope(), add = TRUE)
  testthat::expect_silent(weasel_logo())
  testthat::expect_silent(result <- weasel_example(seed = 42))
  testthat::expect_s3_class(result$plan, "weasel_plan")
  testthat::expect_gt(nrow(result$data), 0)
  testthat::expect_gt(nrow(result$sensitivity), 0)
  testthat::expect_silent(value <- weasel_print_table(
    data.frame(a = c(1.234, 2.345)), digits = 1, n = 1
  ))
  testthat::expect_identical(value, data.frame(a = 1.2))
})

testthat::test_that("informational messages preserve literal braces", {
  old <- options(weasel.verbose = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::expect_message(weasel:::.weasel_msg("{literal}"), "\\{literal\\}")
})

testthat::test_that("display and generation work with a locked global workspace", {
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  code <- quote(local({
    .libPaths(commandArgs(TRUE))
    ns <- loadNamespace("weasel")
    options(weasel.verbose = FALSE)
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    before <- ls(globalenv(), all.names = TRUE)
    lockEnvironment(globalenv(), bindings = TRUE)
    ns$weasel_logo()
    ns$generate_weasel_dummy_data(n_ids = 4, n_times = 5, seed = 7)
    ns$generate_weasel_dummy_data(n_ids = 4, n_times = 5)
    ns$weasel_example(seed = 42)
    stopifnot(identical(ls(globalenv(), all.names = TRUE), before))
    quit(status = 0L)
  }))
  writeLines(deparse(code), script)
  executable <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), executable),
    c("--vanilla", shQuote(script), shQuote(.libPaths())),
    stdout = TRUE, stderr = TRUE
  ))
  testthat::expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})

testthat::test_that("failed generation removes its temporary files", {
  before <- list.files(tempdir(), pattern = "^weasel-dummy-", full.names = TRUE)
  testthat::expect_error(
    weasel:::.weasel_dummy_process(list()), "separate R process failed",
    class = "weasel_error"
  )
  after <- list.files(tempdir(), pattern = "^weasel-dummy-", full.names = TRUE)
  testthat::expect_identical(after, before)
})
