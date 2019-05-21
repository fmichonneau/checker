context("make sure that data-checker-ignore attributes are being ignored")


test_that("data-checker-ignore works", {
  res_ignore_attr <- check_links(
    dir = "./html_files",
    regexp = "test_checker_ignore.html",
    show_summary = FALSE,
    only_with_issues = FALSE
  )

  expect_true(all(res_ignore_attr$error_level == "success"))
  expect_true(all(!grepl("should be ignored",
    res_ignore_attr$link_text, ignore.case = TRUE)))
})
