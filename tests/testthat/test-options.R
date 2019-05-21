context("modify defaults")

test_that("modify defaults with .Rprofile", {
  res <- withr::with_options(
    list(checker.options = list(status_code_404 = "ok")), {
      check_links(dir = "html_files", regexp = "quick_broken.html",
        show_summary = FALSE, only_with_issues = FALSE)
    })
  expect_true(res$error_level[res$link_text == "external broken link"] == "ok")
})


test_that("modify defaults with argument", {
  res <- check_links(dir = "html_files", regexp = "quick_broken.html",
    show_summary = FALSE, only_with_issues = FALSE,
    checker_options = list(status_code_404 = "ok"))
  expect_true(res$error_level[res$link_text == "external broken link"] == "ok")
})

test_that("function-specified options override options()", {
  expect_warning(res <- withr::with_options(
    list(checker.options = list(status_code_404 = "ok")), {
      check_links(
        dir = "html_files",
        regexp = "quick_broken.html",
        show_summary = FALSE, only_with_issues = FALSE,
        checker_options = list(status_code_404 = "warning")
      )
    })
  )
})
