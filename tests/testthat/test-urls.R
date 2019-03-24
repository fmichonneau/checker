build_html_page <- function(o, self_contained = TRUE) {
  f <- file.path("rmd_files", "test_urls_img.Rmd")
  rmarkdown::render(
    f,
    output_file = o,
    quiet = TRUE,
    output_options = list(self_contained = self_contained))
}


## Common variables -----------------------------------------------------------

expected_cols <- list(
  "file" = "character",
  "tag_type" = "character",
  "link" = "character",
  "scheme" = "character",
  "link_text" = "character",
  "full_path" = "character",
  "valid" = "logical",
  "message" = "character",
  "alt_text" = "character"
)


## Self-contained files -------------------------------------------------------

context("self contained files")
out_self_contained <- build_html_page(
  "test_self_contained.html",
  self_contained = TRUE)

all_links_self_contained <- check_links(
  dir = dirname(out_self_contained),
  regexp = "test_self_contained.html",
  only_with_issues = FALSE,
  show_summary = FALSE)

with_issues_self_contained <- check_links(
  dir = dirname(out_self_contained),
  regexp = "test_self_contained.html",
  only_with_issues = TRUE,
  show_summary = FALSE)

test_that("output has correct format for self-contained", {
  expect_true(inherits(all_links_self_contained, "tbl_df"))
  expect_true(inherits(with_issues_self_contained, "tbl_df"))
  expect_identical(lapply(all_links_self_contained, class), expected_cols)
  expect_identical(lapply(with_issues_self_contained, class), expected_cols)
  expect_identical(nrow(all_links_self_contained), 24L)
  expect_identical(nrow(with_issues_self_contained), get_n_issues(with_issues_self_contained))
  expect_true(nrow(with_issues_self_contained) >= 4)
})

test_that("404 are working", {
    links_404  <- all_links_self_contained[all_links_self_contained$link_text == "404", ]
    expect_identical(nrow(links_404), 1L)
    expect_identical("HTTP status code: 404", unique(links_404$message))
  })

test_that("internal links are working as expected", {
  expect_false("valid" %in% with_issues_self_contained$link_text)
  expect_true("valid" %in% all_links_self_contained$link_text)
  expect_true("broken" %in% with_issues_self_contained$link_text)
  expect_true("broken" %in% all_links_self_contained$link_text)

  sub_with_issues <- with_issues_self_contained[with_issues_self_contained$link_text == "broken", ]
  expect_identical(nrow(sub_with_issues), 1L)
  expect_match(sub_with_issues$message, "File referenced by URL doesn't exist")

  sub_links <- all_links_self_contained[all_links_self_contained$link_text == "broken", ]
  expect_identical(nrow(sub_links), 1L)
  expect_match(sub_links$message, "File referenced by URL doesn't exist")


  expect_false("local within valid" %in% with_issues_self_contained$link_text)
  expect_true("local within valid" %in% all_links_self_contained$link_text)

  expect_false("local outside valid link valid fragment" %in% with_issues_self_contained$link_text)
  expect_true("local outside valid link valid fragment" %in% all_links_self_contained$link_text)

  expect_true("local outside valid link invalid fragment" %in% with_issues_self_contained$link_text)
  expect_true("local outside valid link invalid fragment" %in% all_links_self_contained$link_text)
})


test_that("external links with fragments", {

  ## Valid links
  sub_links_valid <- all_links_self_contained[all_links_self_contained$link_text == "valid external with valid fragment", ]
  sub_with_issues_valid <- with_issues_self_contained[with_issues_self_contained$link_text == "valid external with valid fragment", ]

  expect_identical(nrow(sub_links_valid), 1L)
  expect_identical(nrow(sub_with_issues_valid), 0L)

  ## Invalid links
  sub_links_invalid <- all_links_self_contained[all_links_self_contained$link_text == "valid external with invalid fragment", ]
  sub_with_issues_invalid <- with_issues_self_contained[with_issues_self_contained$link_text == "valid external with invalid fragment", ]

  expect_identical(nrow(sub_links_invalid), 1L)
  expect_identical(nrow(sub_with_issues_invalid), 1L)


})

test_that("local links with fragments for file that doesn't exist", {
  sub_with_issues_fragment <- with_issues_self_contained[with_issues_self_contained$link_text == "local outside invalid link irrelevant fragment", ]
  expect_identical(nrow(sub_with_issues_fragment), 1L)
  expect_match(sub_with_issues_fragment$message, "Local URL .+ doesn't exist")

  sub_links_fragment <- all_links_self_contained[all_links_self_contained$link_text == "local outside invalid link irrelevant fragment", ]
  expect_identical(nrow(sub_links_fragment), 1L)
  expect_match(sub_links_fragment$message, "Local URL .+ doesn't exist")

})


### mailto: --------------------------------------------------------------------

context("self-contained dealing with mailto:")
test_that("mailto: only appears when `only_with_issues=FALSE`", {
  expect_identical(
    length(grep("^mailto:", all_links_self_contained$full_path)), 1L)
  expect_identical(
    length(grep("^mailto:", with_issues_self_contained$full_path)), 0L)
})

test_that("mailto: has NA for valid and no message", {
  sub_mailto <- all_links_self_contained[grepl("^mailto", all_links_self_contained$full_path), ]

  expect_identical(sub_mailto$valid, NA)
  expect_identical(sub_mailto$message, "")

})

### data URI -------------------------------------------------------------------

context("self-contained data URI")
## not sure what we can test for here...

### valid links ----------------------------------------------------------------

context("self-contained valid links")

test_that("check for status code of valid links + message for fragments", {
  sub_valid <- all_links_self_contained[
    all_links_self_contained$valid & !is.na(all_links_self_contained$valid), ]
  expect_true(length(grep("HTTP status code: 200", sub_valid$message)) > 1)
  expect_true(length(grep("Fragment .+ checked and found", sub_valid$message)) > 1)
  expect_true(length(grep("File exists", sub_valid$message)) > 0)
})


###### -------------------------------------------------------------------------
## not self-contained files ----------------------------------------------------
###### -------------------------------------------------------------------------

context("not self-contained files")

out_not_contained <- build_html_page(
  "test_not_contained.html",
  self_contained = FALSE)

all_links_not_contained <- check_links(
  dir = dirname(out_not_contained),
  regexp = "test_not_contained.html",
  only_with_issues = FALSE,
  show_summary = FALSE)

with_issues_not_contained <- check_links(
  dir = dirname(out_not_contained),
  regexp = "test_not_contained.html",
  only_with_issues = TRUE,
  show_summary = FALSE)

test_that("output has correct format for not contained", {
  expect_true(inherits(all_links_not_contained, "tbl_df"))
  expect_true(inherits(with_issues_not_contained, "tbl_df"))
  expect_identical(lapply(all_links_not_contained, class), expected_cols)
  expect_identical(lapply(with_issues_not_contained, class), expected_cols)
  expect_identical(nrow(all_links_not_contained), 31L)
  expect_identical(nrow(with_issues_not_contained), get_n_issues(with_issues_not_contained))
  expect_true(nrow(with_issues_not_contained) >= 4)
})

test_that("404 are working", {
  links_404  <- all_links_not_contained[all_links_not_contained$link_text == "404", ]
  expect_identical(nrow(links_404), 1L)
  expect_identical("HTTP status code: 404", unique(links_404$message))
})

test_that("internal links are working as expected", {
  expect_false("valid" %in% with_issues_not_contained$link_text)
  expect_true("valid" %in% all_links_not_contained$link_text)
  expect_true("broken" %in% with_issues_not_contained$link_text)
  expect_true("broken" %in% all_links_not_contained$link_text)

  sub_with_issues <- with_issues_not_contained[with_issues_not_contained$link_text == "broken", ]
  expect_identical(nrow(sub_with_issues), 1L)
  expect_match(sub_with_issues$message, "File referenced by URL doesn't exist")

  sub_links <- all_links_not_contained[all_links_not_contained$link_text == "broken", ]
  expect_identical(nrow(sub_links), 1L)
  expect_match(sub_links$message, "File referenced by URL doesn't exist")


  expect_false("local within valid" %in% with_issues_not_contained$link_text)
  expect_true("local within valid" %in% all_links_not_contained$link_text)

  expect_false("local outside valid link valid fragment" %in%
                 with_issues_not_contained$link_text)
  expect_true("local outside valid link valid fragment" %in%
                all_links_not_contained$link_text)

  expect_true("local outside valid link invalid fragment" %in% with_issues_not_contained$link_text)
  expect_true("local outside valid link invalid fragment" %in% all_links_not_contained$link_text)

})

test_that("external links with fragments", {

  ## Valid links
  sub_links_valid <- all_links_not_contained[all_links_not_contained$link_text == "valid external with valid fragment", ]
  sub_with_issues_valid <- with_issues_not_contained[with_issues_not_contained$link_text == "valid external with valid fragment", ]

  expect_identical(nrow(sub_links_valid), 1L)
  expect_identical(nrow(sub_with_issues_valid), 0L)

  ## Invalid links
  sub_links_invalid <- all_links_not_contained[all_links_not_contained$link_text == "valid external with invalid fragment", ]
  sub_with_issues_invalid <- with_issues_not_contained[with_issues_not_contained$link_text == "valid external with invalid fragment", ]

  expect_identical(nrow(sub_links_invalid), 1L)
  expect_identical(nrow(sub_with_issues_invalid), 1L)

})

test_that("local links with fragments for file that doesn't exist", {
  sub_with_issues_fragment <- with_issues_not_contained[with_issues_not_contained$link_text == "local outside invalid link irrelevant fragment", ]
  expect_identical(nrow(sub_with_issues_fragment), 1L)
  expect_match(sub_with_issues_fragment$message, "Local URL .+ doesn't exist")

  sub_links_fragment <- all_links_not_contained[all_links_not_contained$link_text == "local outside invalid link irrelevant fragment", ]
  expect_identical(nrow(sub_links_fragment), 1L)
  expect_match(sub_links_fragment$message, "Local URL .+ doesn't exist")

})

### mailto: --------------------------------------------------------------------

context("not contained dealing with mailto:")
test_that("mailto: only appears when `only_with_issues=FALSE`", {
  expect_identical(
    length(grep("^mailto:", all_links_not_contained$full_path)), 1L)
  expect_identical(
    length(grep("^mailto:", with_issues_not_contained$full_path)), 0L)
})

test_that("mailto: has NA for valid and no message", {
  sub_mailto <- all_links_not_contained[grepl("^mailto", all_links_not_contained$full_path), ]

  expect_identical(sub_mailto$valid, NA)
  expect_identical(sub_mailto$message, "")

})

### data URI -------------------------------------------------------------------

context("not contained data URI")
test_that("data URI only appears when `only_with_issues=FALSE`", {
  expect_identical(
    length(grep("^data:", all_links_not_contained$full_path)), 0L
  )
  expect_identical(
    length(grep("^data:", with_issues_not_contained$full_path)), 0L
  )
})

test_that("data URI has NA for valid", {
  sub_datauri <- all_links_not_contained[grepl("^data:", all_links_not_contained$full_path), ]

  expect_true(all(is.na(sub_datauri$valid)))
  expect_true(all(sub_datauri$message == ""))

})


### valid links ----------------------------------------------------------------

context("not contained valid links")

test_that("check for status code of valid links + message for fragments", {
  sub_valid <- all_links_not_contained[
    all_links_not_contained$valid & !is.na(all_links_not_contained$valid), ]
  expect_true(length(grep("HTTP status code: 200", sub_valid$message)) > 1)
  expect_true(length(grep("Fragment .+ checked and found", sub_valid$message)) > 1)
  expect_true(length(grep("File exists", sub_valid$message)) > 0)
})

###### -------------------------------------------------------------------------
### Pages with no links
###### -------------------------------------------------------------------------

context("page with no links")

no_links_file <- file.path("html_files", "test_no_links.html")

all_links_no_links <- check_links(
  dir = dirname(no_links_file),
  regexp = "test_no_links.html",
  only_with_issues = FALSE,
  show_summary = FALSE
)

with_issues_no_links <- check_links(
  dir = dirname(no_links_file),
  regexp = "test_no_links.html",
  only_with_issues = TRUE,
  show_summary = FALSE
)

test_that("data structure of object return when there is no links is OK", {
  expect_identical(all_links_no_links, with_issues_no_links)
  expect_identical(lapply(all_links_no_links, class), expected_cols)
  expect_identical(lapply(with_issues_no_links, class), expected_cols)
  expect_identical(nrow(all_links_no_links), 0L)
  expect_identical(nrow(with_issues_no_links), 0L)
})

###### -------------------------------------------------------------------------
### Pages with no broken links
###### -------------------------------------------------------------------------

context("page with no broken links")

no_broken_file <- file.path("html_files", "test_all_valid.html")

all_links_no_broken <- check_links(
  dir = dirname(no_broken_file),
  regexp = no_broken_file,
  only_with_issues = FALSE,
  show_summary = FALSE
)

broken_no_broken <- check_links(
  dir = dirname(no_broken_file),
  regexp = no_broken_file,
  only_with_issues = TRUE,
  show_summary = FALSE
)

test_that("valid values are all TRUE", {
  expect_identical(
    nrow(all_links_no_broken), 4L
  )
  expect_true(all(all_links_no_broken$valid))
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
    check_links(dir = dirname(no_broken_file), regexp = "^foobar$",
      show_summary = FALSE)
  )
})

test_that("warning when no file match the glob", {
  expect_warning(
    check_links(dir = dirname(no_broken_file),
      regexp = "*alongstringnotfoundinfolder*",
      show_summary = FALSE)
  )
})

test_that("error when both glob and regexp are specified", {
  expect_error(
    ## throws error because of default value set to regexp
    check_links(dir = dirname(no_broken_file), glob = "foo",
      show_summary = FALSE)
  )
  expect_error(
    check_links(dir = dirname(no_broken_file),
      glob = "foo", regexp = "bar",
      show_summary = FALSE
    )
  )
})

context("compare regexp and glob")

test_that("regexp and glob give the same result", {
  with_glob <- check_links(dir = dirname(no_broken_file),
    glob = "*_all_valid.html", regexp = NULL,
    only_with_issues = FALSE,
    show_summary = FALSE)

  with_regexp <- check_links(dir = dirname(no_broken_file),
    regexp = "_all_valid.html$",
    only_with_issues = FALSE,
    show_summary = FALSE)

  expect_identical(with_glob, with_regexp)

})


##### --------------------------------------------------------------------------
##### Check raise levels
##### --------------------------------------------------------------------------


##### --------------------------------------------------------------------------
##### Test different types of outputs
##### --------------------------------------------------------------------------
