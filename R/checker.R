##' @importFrom dplyr %>%
##' @importFrom rlang .data

##' @title Check links in your documents
##'
##' Currently only HTML files are supported.
##'
##' @details Data URI and \code{mailto:} links are not checked.
##'
##' The \code{by} argument controls how the summary of the results is being
##' displayed. Using \code{page} is typically more convenient for small sites,
##' while \code{resource} works better for larger websites that use templates
##' are more likely to have mispecified resources across many pages.
##'
##' @param dir The directory to look for documents
##' @param recursive Should sub-folders be searched for documents? (default
##'   `TRUE`).
##' @param regexp A regular expression matching the names of the files to check.
##' @param glob A wildcard pattern matching the names fo the files to check.
##' @param only_with_issues Should the results include only the broken links
##'   (default) or also the valid links?
##' @param raise If set to `warning` or `error`, the function will raise a
##'   warning or an error if broken links are found.
##' @param by How should the results of the checks be aggregated?
##' @param show_summary Should a summary of the results be printed?
##' @param ... additional parameters to be passed to `grep` to match the file
##'   names to check.
##' @return a tibble with the name of the file that includes the link, the link,
##'   the expanded full path (useful for local/relative links), whether the link
##'   is valid, and possibly the message/HTTP code returned by the server.
##' @importFrom fs dir_ls
##' @importFrom purrr map_df invoke_map
##' @importFrom dplyr distinct mutate group_by case_when left_join select filter
##' @importFrom tidyr nest unnest
##' @export
check_links <- function(dir = ".", recursive = TRUE,
                        regexp = "\\.html?$", glob = NULL,
                        only_with_issues = TRUE,
                        raise = c("ok", "warning", "error"),
                        by = c("page", "resource"),
                        show_summary = TRUE, ...) {

  raise <- match.arg(raise)
  by <- match.arg(by)

  links <- extract_all_links(dir = dir, recursive = recursive,
    regexp = regexp, glob = glob, ...)

  if (identical(nrow(links), 0L)) {
    return(tibble::tibble(
      file = character(0),
      tag_type = character(0),
      link = character(0),
      scheme = character(0),
      link_text = character(0),
      full_path = character(0),
      valid = logical(0),
      message = character(0),
      alt_text = character(0)
    ))
  }

  uniq_links <- dplyr::distinct(links, .data$uri_type, .data$full_path)

  res <- uniq_links %>%
    dplyr::group_by(.data$uri_type) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        uri_type == "local" ~ "check_local_file",
        uri_type == "external" ~ "check_url",
        uri_type == "data" ~ "check_data",
        uri_type %in% c("mailto", "news") ~ "no_check",
        TRUE ~ "unknown_protocol"
      )) %>%
    dplyr::mutate(
      res = purrr::invoke_map(.data$fn, .data$data)
    ) %>%
    tidyr::unnest()

  out <- dplyr::left_join(links, res, by = c("full_path", "uri_type"))

  out <- out %>%
    check_fragments() %>%
    dplyr::select(
      .data$file,
      .data$tag_type,
      .data$link,
      .data$scheme,
      .data$link_text,
      .data$full_path,
      .data$valid,
      .data$message,
      .data$alt_text
    )

  if (only_with_issues) {
    out <- out %>%
      dplyr::filter(
        has_issues_assertion(.data)
      )
  }

  if (show_summary) {
    summary_check_images(out)
    summary_check_links(out, by)
  }

  handle_raise(out, raise)

  invisible(out)

}


