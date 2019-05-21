
##### --------------------------------------------------------------------------
##### Check raise levels + options
##### --------------------------------------------------------------------------
no_broken_file <- file.path("html_files", "test_all_valid.html")
quick_broken_file <- file.path("html_files", "quick_broken.html")

context("check stop_on_error = TRUE")

test_that("check `stop_on_error = TRUE` with no broken links", {
  expect_silent(
    all_links_no_broken <- check_links(
      dir = dirname(no_broken_file),
      regexp = no_broken_file,
      only_with_issues = FALSE,
      show_summary = FALSE,
      stop_on_error = TRUE
    )
  )

  expect_silent(
    with_issues_no_broken <- check_links(
      dir = dirname(no_broken_file),
      regexp = no_broken_file,
      only_with_issues = TRUE,
      show_summary = FALSE,
      stop_on_error = TRUE
    )
  )
})

test_that("check `stop_on_error = FALSE` with no broken links", {
  expect_silent(
    all_links_no_broken <- check_links(
      dir = dirname(no_broken_file),
      regexp = no_broken_file,
      only_with_issues = FALSE,
      show_summary = FALSE,
      stop_on_error = FALSE
    )
  )

  expect_silent(
    with_issues_no_broken <- check_links(
      dir = dirname(no_broken_file),
      regexp = no_broken_file,
      only_with_issues = TRUE,
      show_summary = FALSE,
      stop_on_error = FALSE
    )
  )
})

test_that("check `stop_on_error = TRUE` with broken links", {
  expect_error(
    all_links_quick_broken <- check_links(
      dir = dirname(quick_broken_file),
      regexp = quick_broken_file,
      only_with_issues = FALSE,
      show_summary = FALSE,
      stop_on_error = TRUE
    )
  )

  expect_error(
    with_issues_quick_broken <- check_links(
      dir = dirname(quick_broken_file),
      regexp = quick_broken_file,
      only_with_issues = TRUE,
      show_summary = FALSE,
      stop_on_error = TRUE
    )
  )
})

test_that("check `stop_on_error = FALSE` with broken links", {
  expect_message(
    all_links_quick_broken <- check_links(
      dir = dirname(quick_broken_file),
      regexp = quick_broken_file,
      only_with_issues = FALSE,
      show_summary = FALSE,
      stop_on_error = FALSE
    )
  )

  expect_message(
    with_issues_quick_broken <- check_links(
      dir = dirname(quick_broken_file),
      regexp = quick_broken_file,
      only_with_issues = TRUE,
      show_summary = FALSE,
      stop_on_error = FALSE
    )
  )
})
