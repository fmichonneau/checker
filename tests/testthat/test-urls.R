build_html_page <- function(o, self_contained = TRUE) {
  f <- system.file("html_files", "test_urls_img.Rmd", package = "checker")
  rmarkdown::render(
    f,
    output_file = o,
    output_options = list(self_contained = self_contained))
}


### Common variables -----------------------------------------------------------

expected_cols <- list(
  "file" = "character",
  "link" = "character",
  "link_text" = "character",
  "full_path" = "character",
  "valid" = "logical",
  "message" = "character"
)


### Self-contained files -------------------------------------------------------

context("self contained files")
out_self_contained <- build_html_page(
  "test_self_contained.html",
  self_contained = TRUE)

links_self_contained <- check_links(
  dir = dirname(out_self_contained),
  regexp = "test_self_contained.html",
  only_broken = FALSE)

broken_self_contained <- check_links(
  dir = dirname(out_self_contained),
  regexp = "test_self_contained.html",
  only_broken = TRUE)

test_that("output has correct format", {
  expect_true(inherits(links_self_contained, "tbl_df"))
  expect_true(inherits(broken_self_contained, "tbl_df"))
  expect_identical(lapply(links_self_contained, class), expected_cols)
  expect_identical(lapply(broken_self_contained, class), expected_cols)
  expect_identical(nrow(links_self_contained), 19L)
  expect_identical(nrow(broken_self_contained), get_n_broken(broken_self_contained))
  expect_true(nrow(broken_self_contained) >= 4)
})

test_that("404 are working", {
  links_404  <- links_self_contained[links_self_contained$link_text == "404", ]
  expect_identical(nrow(links_404), 1gL)
  expect_identical("HTTP status code: 404", unique(links_404$message))
})


context("file with external resources")
