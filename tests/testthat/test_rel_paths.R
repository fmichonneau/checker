context("test for paths")

res_rel_paths <- check_links(
  dir = "html_rel_paths",
  show_summary = FALSE,
  only_with_issues = FALSE
)

test_that("only broken links are broken", {
  sub_valid  <- res_rel_paths[res_rel_paths$error_level == -1L, ]
  sub_broken <- res_rel_paths[res_rel_paths$error_level ==  3L, ]
  expect_true(all(!grepl("broken", sub_valid$link_text)))
  expect_true(all(grepl("broken", sub_broken$link_text)))
})
