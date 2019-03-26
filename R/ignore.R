##' @importFrom dplyr filter

filter_ignore_pattern <- function(.dt, ignore_pattern) {
  if (!is.null(ignore_pattern)) {
    for (i in seq_along(ignore_pattern)) {
      .dt <- .dt %>%
        dplyr::filter(!grepl(ignore_pattern[i], .data$full_path))
    }
  }
  .dt
}


filter_ignore_tag <- function(.dt, ignore_tag) {
  if (!is.null(ignore_tag)) {
    .dt <- .dt %>%
      dplyr::filter(!.data$tag_type %in% tolower(ignore_tag))
  }
  .dt
}

filter_external <- function(.dt, check_external) {
  if (!check_external) {
    .dt <- .dt %>%
      dplyr::filter(.data$uri_type != "external")
  }
  .dt
}
