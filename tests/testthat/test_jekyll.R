has_bundle <- function() {
  test_bundle <- processx::run("which", "bundle", error_on_status = FALSE)

  if (identical(test_bundle$status, 0L) &&
        nzchar(test_bundle$stdout))
    TRUE
  else FALSE
}

context("Jekyll: only broken links are reported broken")
if (has_bundle()) {

  jkyl <- withr::with_dir(
    "jekyll_site/testsite/", {
      processx::run("bundle", c("update", "--local"))
      processx::process$new(
        "bundle",
        c("exec", "jekyll", "serve", "--port", "4001"),
        echo = TRUE, stdout = "|", stderr = "|")
    })

  jkyl$wait(2000)

  vv <- jkyl$read_output_lines()
  if (!any(grepl("Server running", vv))) stop("jekyll not running")

  res_jekyll <- check_links(
    "jekyll_site/testsite/_site",
    root_dir = "http://localhost:4001",
    recurse = TRUE,
    only_with_issues = FALSE,
    show_summary = FALSE
  )

  test_that("functional links are functional", {
    sub_functional <- res_jekyll[grepl("functional", res_jekyll$link_text), ]
    expect_true(all(sub_functional$error_level == "success"))
  })


  test_that("broken links are broken", {
    sub_broken <- res_jekyll[grepl("broken", res_jekyll$link_text), ]
    expect_true(all(sub_broken$error_level == "error"))
  })

  jkyl$kill()

} else {
  skip("bundle not available to test jekyll site.")
}
