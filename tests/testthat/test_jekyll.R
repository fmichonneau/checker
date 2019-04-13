has_bundle <- function() {
  test_bundle <- processx::run("which", "bundle", error_on_status = FALSE)

  if (identical(test_bundle$status, 0L) &&
        nzchar(test_bundle$stdout))
    TRUE
  else FALSE
}

if (has_bundle()) {

  jkyl <- withr::with_dir(
    "jekyll_site/testsite/", {
      processx::run("bundle", "update")
      processx::process$new(
        "bundle",
        c("exec", "jekyll", "serve", "--port", "4001"),
        echo = TRUE)
    })

  jkyl$wait()

  res_jekyll <- check_links(
    "jekyll_site/testsite/_site",
    root_dir = "http://localhost:4001",
    recursive= TRUE,
    only_with_issues = FALSE,
    show_summary = FALSE
  )

  context("Jekyll: only broken links are reported broken")

  test_that("functional links are functional", {
    sub_functional <- res_jekyll[grepl("functional", res_jekyll$link_text), ]
    expect_true(all(sub_functional$valid))
  })


  test_that("broken links are broken", {
    sub_broken <- res_jekyll[grepl("broken", res_jekyll$link_text), ]
    expect_true(all(!sub_broken$valid))
  })

  jkyl$kill()

} else {
  skip("bundle not available to test jekyll site.")
}
