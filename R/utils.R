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

##' @importFrom dplyr mutate
make_path_rel <- function(.data, dir, show_full_path) {
  if (show_full_path)
    return(.data)

  dir <- normalizePath(dir)

  .data %>%
    dplyr::mutate(
      file = gsub(dir, ".", .data$file)
    )
}


##' @importFrom dplyr case_when
get_uri_type <- function(scheme, server, ...) {
  dplyr::case_when(
    scheme == "data" ~ "data",
    scheme == "mailto" ~ "mailto",
    scheme == "" & server == "" ~ "local",
    TRUE ~ "external"
  )
}
