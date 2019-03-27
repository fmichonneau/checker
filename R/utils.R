broken_link_assertion <- function(.dt) {
  !.dt$valid
}

has_issues_assertion <- function(.dt) {
  broken_link_assertion(.dt) |
    (.dt$tag_type == "img" & is.na(.dt$alt_text)) |
    (.dt$tag_type == "img" & .dt$scheme == "http")
}

get_n_issues <- function(.dt) {
  sum(has_issues_assertion(.dt), na.rm = TRUE)
}


get_n_broken <- function(.dt) {
  sum(broken_link_assertion(.dt), na.rm = TRUE)
}

get_n_valid <- function(.dt) {
  sum(!broken_link_assertion(.dt), na.rm = TRUE)
}


convert_data_uri <- function(data_uri) {
  paste0(substr(data_uri, 1, 100), "...")
}

##' @importFrom dplyr case_when
get_uri_type <- function(scheme, server, ...) {
  dplyr::case_when(
    scheme == "data" ~ "data",
    scheme == "mailto" ~ "mailto",
    scheme == "" & server == "" ~ "local",
    scheme == "http" & server %in% c("localhost", "127.0.0.1") ~ "localhost",
    TRUE ~ "external"
  )
}

handle_raise <- function(out, raise) {

  msg <- "Broken links found."

  if (get_n_broken(out) > 0) {
    switch(raise,
      ok = NULL,
      warning = warning(msg, call. = FALSE),
      error = stop(msg, call. = FALSE))
  }
}

##' @importFrom fs is_dir
assert_dir <- function(dir) {
  if (!identical(length(dir), 1L))
    stop("Make sure you only provide 1 path", call. = FALSE)
  if (!fs::is_dir(dir))
    stop(sQuote(dir), " doesn't seem to be a directory", call. = FALSE)
}
