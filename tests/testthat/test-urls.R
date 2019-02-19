build_html_page <- function(o, self_contained = TRUE) {
  f <- system.file("html_files", "test_urls_img.Rmd", package = "checker")
  rmarkdown::render(
    f,
    output_file = o,
    output_options = list(self_contained = self_contained))
}


## Common variables -----------------------------------------------------------

expected_cols <- list(
  "file" = "character",
  "link" = "character",
  "link_text" = "character",
  "full_path" = "character",
  "valid" = "logical",
  "message" = "character"
)


## Self-contained files -------------------------------------------------------

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

test_that("output has correct format for self-contained", {
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
    expect_identical(nrow(links_404), 1L)
    expect_identical("HTTP status code: 404", unique(links_404$message))
  })

test_that("internal links are working as expected", {
  expect_false("valid" %in% broken_self_contained$link_text)
  expect_true("valid" %in% links_self_contained$link_text)
  expect_true("broken" %in% broken_self_contained$link_text)
  expect_true("broken" %in% links_self_contained$link_text)

  sub_broken <- broken_self_contained[broken_self_contained$link_text == "broken", ]
  expect_identical(nrow(sub_broken), 1L)
  expect_match(sub_broken$message, "File referenced by URL doesn't exist")

  sub_links <- links_self_contained[links_self_contained$link_text == "broken", ]
  expect_identical(nrow(sub_links), 1L)
  expect_match(sub_links$message, "File referenced by URL doesn't exist")


  expect_false("local within valid" %in% broken_self_contained$link_text)
  expect_true("local within valid" %in% links_self_contained$link_text)

  expect_false("local outside valid" %in% broken_self_contained$link_text)
  expect_true("local outside valid" %in% links_self_contained$link_text)



})

### mailto: --------------------------------------------------------------------

context("self-contained dealing with mailto:")
test_that("mailto: only appears when `only_broken=FALSE`", {
  expect_identical(
    length(grep("^mailto:", links_self_contained$full_path)), 1L)
  expect_identical(
    length(grep("^mailto:", broken_self_contained$full_path)), 0L)
})

test_that("mailto: has NA for valid and no message", {
  sub_mailto <- links_self_contained[grepl("^mailto", links_self_contained$full_path), ]

  expect_identical(sub_mailto$valid, NA)
  expect_identical(sub_mailto$message, "")

})

### data URI -------------------------------------------------------------------

context("self-contained data URI")
test_that("data URI only appears when `only_broken=FALSE`", {
  expect_true(
    length(grep("^<data URI>", links_self_contained$full_path)) > 1
  )
  expect_identical(
    length(grep("^<data URI>", broken_self_contained$full_path)), 0L
  )
})

test_that("data URI has NA for valid", {
  sub_datauri <- links_self_contained[grepl("^<data URI>", links_self_contained$full_path), ]

  expect_true(all(is.na(sub_datauri$valid)))
  expect_true(all(sub_datauri$message == ""))

})


### valid links ----------------------------------------------------------------

context("self-contained valid links")

test_that("check for status code of valid links + message for fragments", {
  sub_valid <- links_self_contained[
    links_self_contained$valid & !is.na(links_self_contained$valid), ]
  expect_true(length(grep("HTTP status code: 200", sub_valid$message)) > 1)
  expect_true(length(grep("Fragment .+ checked and found", sub_valid$message)) > 1)
  expect_true(length(grep("File exists", sub_valid$message)) > 1)
})


###### -------------------------------------------------------------------------
## not self-contained files ----------------------------------------------------
###### -------------------------------------------------------------------------

context("self contained files")

out_not_contained <- build_html_page(
  "test_not_contained.html",
  self_contained = TRUE)

links_not_contained <- check_links(
  dir = dirname(out_not_contained),
  regexp = "test_not_contained.html",
  only_broken = FALSE)

broken_not_contained <- check_links(
  dir = dirname(out_not_contained),
  regexp = "test_not_contained.html",
  only_broken = TRUE)

test_that("output has correct format for notcontained", {
  expect_true(inherits(links_not_contained, "tbl_df"))
  expect_true(inherits(broken_not_contained, "tbl_df"))
  expect_identical(lapply(links_not_contained, class), expected_cols)
  expect_identical(lapply(broken_not_contained, class), expected_cols)
  expect_identical(nrow(links_not_contained), 19L)
  expect_identical(nrow(broken_not_contained), get_n_broken(broken_not_contained))
  expect_true(nrow(broken_not_contained) >= 4)
})

test_that("404 are working", {
  links_404  <- links_not_contained[links_not_contained$link_text == "404", ]
  expect_identical(nrow(links_404), 1L)
  expect_identical("HTTP status code: 404", unique(links_404$message))
})

test_that("internal links are working as expected", {
  expect_false("valid" %in% broken_not_contained$link_text)
  expect_true("valid" %in% links_not_contained$link_text)
  expect_true("broken" %in% broken_not_contained$link_text)
  expect_true("broken" %in% links_not_contained$link_text)

  sub_broken <- broken_not_contained[broken_not_contained$link_text == "broken", ]
  expect_identical(nrow(sub_broken), 1L)
  expect_match(sub_broken$message, "File referenced by URL doesn't exist")

  sub_links <- links_not_contained[links_not_contained$link_text == "broken", ]
  expect_identical(nrow(sub_links), 1L)
  expect_match(sub_links$message, "File referenced by URL doesn't exist")


  expect_false("local within valid" %in% broken_not_contained$link_text)
  expect_true("local within valid" %in% links_not_contained$link_text)

  expect_false("local outside valid" %in% broken_not_contained$link_text)
  expect_true("local outside valid" %in% links_not_contained$link_text)
})

### mailto: --------------------------------------------------------------------

context("not contained dealing with mailto:")
test_that("mailto: only appears when `only_broken=FALSE`", {
  expect_identical(
    length(grep("^mailto:", links_not_contained$full_path)), 1L)
  expect_identical(
    length(grep("^mailto:", broken_not_contained$full_path)), 0L)
})

test_that("mailto: has NA for valid and no message", {
  sub_mailto <- links_not_contained[grepl("^mailto", links_not_contained$full_path), ]

  expect_identical(sub_mailto$valid, NA)
  expect_identical(sub_mailto$message, "")

})

### data URI -------------------------------------------------------------------

context("not contained data URI")
test_that("data URI only appears when `only_broken=FALSE`", {
  expect_true(
    length(grep("^<data URI>", links_not_contained$full_path)) > 1
  )
  expect_identical(
    length(grep("^<data URI>", broken_not_contained$full_path)), 0L
  )
})

test_that("data URI has NA for valid", {
  sub_datauri <- links_not_contained[grepl("^<data URI>", links_not_contained$full_path), ]

  expect_true(all(is.na(sub_datauri$valid)))
  expect_true(all(sub_datauri$message == ""))

})


### valid links ----------------------------------------------------------------

context("not contained valid links")

test_that("check for status code of valid links + message for fragments", {
  sub_valid <- links_not_contained[
    links_not_contained$valid & !is.na(links_not_contained$valid), ]
  expect_true(length(grep("HTTP status code: 200", sub_valid$message)) > 1)
  expect_true(length(grep("Fragment .+ checked and found", sub_valid$message)) > 1)
  expect_true(length(grep("File exists", sub_valid$message)) > 1)
})

###### -------------------------------------------------------------------------
### Pages with no links
###### -------------------------------------------------------------------------

context("page with no links")

no_links_file <- system.file("html_files", "test_no_links.html",
  package = "checker")

links_no_links <- check_links(dir = dirname(no_links_file),
  regexp = "test_no_links.html$", only_broken = FALSE)

broken_no_links <- check_links(dir = dirname(no_links_file),
  regexp = "test_no_links.html$", only_broken = TRUE)

test_that("data structure of object return when there is no links is OK", {
  expect_identical(links_no_links, broken_no_links)
  expect_identical(lapply(links_no_links, class), expected_cols)
  expect_identical(lapply(broken_no_links, class), expected_cols)
  expect_identical(nrow(links_no_links), 0L)
  expect_identical(nrow(broken_no_links), 0L)
})


###### -------------------------------------------------------------------------
### Pages with no links
###### -------------------------------------------------------------------------

context("page with no links")

no_links_file <- system.file("html_files", "test_no_links.html",
  package = "checker")

links_no_links <- check_links(dir = dirname(out_self_contained),
  regexp = "test_no_links.html$", only_broken = FALSE)

broken_no_links <- check_links(dir = dirname(out_self_contained),
  regexp = "test_no_links.html$", only_broken = TRUE)

test_that("data structure of object return when there is no links is OK", {
  expect_identical(links_no_links, broken_no_links)
  expect_identical(lapply(links_no_links, class), expected_cols)
  expect_identical(lapply(broken_no_links, class), expected_cols)
  expect_identical(nrow(links_no_links), 0L)
  expect_identical(nrow(broken_no_links), 0L)
})

###### -------------------------------------------------------------------------
### Pages with no broken links
###### -------------------------------------------------------------------------

context("page with no broken links")

no_broken_file <- system.file("html_files", "test_all_valid.html",
  package = "checker")

links_no_broken <- check_links(dir = dirname(no_broken_file),
  regexp = no_broken_file, only_broken = FALSE)

broken_no_broken <- check_links(dir = dirname(no_broken_file),
  regexp = no_broken_file, only_broken = TRUE)

test_that("valid values are all TRUE", {
  expect_identical(
    nrow(links_no_broken), 4L
  )
  expect_true(all(links_no_broken$valid))
})

test_that("empty tibble when there are no broken links", {
  expect_identical(
    nrow(broken_no_broken), 0L
  )
})


###### -------------------------------------------------------------------------
### Invalid regexp or glob
###### -------------------------------------------------------------------------

context("invalid regexp or glob")

test_that("warning is returned when no file match the regexp", {
  expect_warning(
    check_links(dir = dirname(no_broken_file), regexp = "^foobar$")
  )
})

test_that("warning when no file match the glob", {
  expect_warning(
    check_links(dir = dirname(no_broken_file), regexp = "*foo*")
  )
})

test_that("error when both glob and regexp are specified", {
  expect_error(
    ## throws error because of default value set to regexp
    check_links(dir = dirname(no_broken_file), glob = "foo")
  )
  expect_error(
    check_links(dir = dirname(no_broken_file),
      glob = "foo", regexp = "bar"
    )
  )
})

context("compare regexp and glob")

test_that("regexp and glob give the same result", {
  with_glob <- check_links(dir = dirname(no_broken_file),
    glob = "*_all_valid.html", regexp = NULL,
    only_broken = FALSE)

  with_regexp <- check_links(dir = dirname(no_broken_file),
    regexp = "_all_valid.html$",
    only_broken = FALSE)

  expect_identical(with_glob, with_regexp)

})
