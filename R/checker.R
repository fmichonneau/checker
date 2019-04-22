##' @importFrom dplyr %>%
##' @importFrom rlang .data

##' @title Check links in your documents
##'
##' @details Data URI and \code{mailto:} links are not checked.
##'
##' The \code{by} argument controls how the summary of the results is being
##' displayed. Using \code{page} is typically more convenient for small sites,
##' while \code{resource} works better for larger websites that use templates
##' are more likely to have mispecified resources across many pages.
##'
##' `checker` attempts to respect the rules specified by the robots.txt files
##' provided by the external servers. Thus some URLs might not be checked.
##'
##' @param dir The directory to look for documents
##' @param recursive Should sub-folders be searched for documents? (default
##'   `TRUE`).
##' @param regexp A regular expression matching the names of the files to check.
##' @param glob A wildcard pattern matching the names of the files to check.
##' @param root_dir The path for the root of the website. By default, the same
##'   value as `dir`, but can be overriden to use another path (if testing only
##'   some files within the directory structure of the site) or a webserver
##'   address (e.g., `http://localhost:4000`, if testing a Jekyll site).
##' @param ignore_pattern A vector of regular expressions matching the path of
##'   the links to ignore in the files (see Details).
##' @param ignore_tag A vector of HTML tags to ignore.
##' @param check_external Should external links be checked? If `FALSE`, only
##'   local links will be checked.
##' @param only_with_issues Should the results include only the broken links
##'   (default) or also the valid links?
##' @param raise If set to `warning` or `error`, the function will raise a
##'   warning or an error if broken links are found.
##' @param by How should the results of the checks be aggregated?
##' @param show_summary Should a summary of the results be printed?
##' @param checker_options An optional list that specifies which situations
##'   checker should consider invalid. See \code{\link{checker_options}}.
##' @param ... additional parameters to be passed to `grep` to match the file
##'   names to check.
##' @details
##'   ## Ignore pattern
##'
##'   If more than one regular expressions is provided to `ignore_pattern`, they
##'   will be evaluated in succession: thus, the order in which you provide them
##'   may matter. For local files, matching of the regular expressions is done
##'   on the fully expanded link paths. Make sure your regular expression don't
##'   inadvertently match patterns higher up in your tree files to the risk of
##'   excluding all files from being checked.
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
                        root_dir = dir,
                        ignore_pattern = NULL,
                        ignore_tag = NULL,
                        check_external = TRUE,
                        only_with_issues = TRUE,
                        raise = c("ok", "warning", "error"),
                        by = c("page", "resource"),
                        show_summary = TRUE,
                        checker_options = NULL, ...) {

  raise <- match.arg(raise)
  by <- match.arg(by)

  ## `dir` must be local directory
  assert_dir(dir)
  dir <- normalizePath(dir)

  ## `root_dir` may not be a local directory but it needs to be of length 1
  if (!identical(length(root_dir), 1L)) {
    stop(sQuote("root_dir"), " argument must be a single element. It has: ",
      length(root_dir), ".")
  }
  if (fs::is_dir(root_dir)) {
    root_dir <- normalizePath(root_dir, mustWork = TRUE)
  }
  if (!identical(substr(root_dir, nchar(root_dir), nchar(root_dir)), "/")) {
    root_dir <- paste0(root_dir, "/")
  }

  links <- extract_all_links(dir = dir, recursive = recursive,
    regexp = regexp, glob = glob, root_dir = root_dir, ...) %>%
    filter_ignore_pattern(ignore_pattern) %>%
    filter_ignore_tag(ignore_tag) %>%
    filter_external(check_external)

  if (identical(nrow(links), 0L)) {
    return(empty_check_links())
  }

  if (! "is_allowed" %in% names(links))
    links$is_allowed <- NA

  uniq_links <- dplyr::distinct(
    links,
    .data$uri_type,
    .data$full_path,
    .data$is_allowed
  )

  res <- uniq_links %>%
    dplyr::group_by(.data$uri_type, .data$is_allowed) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        uri_type == "local" ~ "check_local_file",
        uri_type == "external" & (is_allowed | is.na(is_allowed)) ~ "check_url_external",
        uri_type == "external" ~ "robotstxt_denied",
        uri_type == "localhost" ~ "check_url_localhost",
        uri_type == "data" ~ "check_data",
        uri_type %in% c("ftp", "ftps", "mailto", "news") ~ "no_check",
        TRUE ~ "unknown_protocol"
      )) %>%
    dplyr::mutate(
      res = purrr::invoke_map(.data$fn, .data$data, checker_options)
    ) %>%
    tidyr::unnest()



  out <- dplyr::left_join(links, res, by = c("full_path", "uri_type"))

  out <- out %>%
    check_fragments(checker_options) %>%
    dplyr::select(
      .data$file,
      .data$tag_type,
      .data$link,
      .data$scheme,
      .data$link_text,
      .data$full_path,
      .data$error_level,
      .data$message,
      .data$alt_text
    ) %>%
    dplyr::arrange(.data$full_path)

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

empty_check_links <- function() {
  tibble::tibble(
    file = character(0),
    tag_type = character(0),
    link = character(0),
    scheme = character(0),
    link_text = character(0),
    full_path = character(0),
    error_level = integer(0),
    message = character(0),
    alt_text = character(0)
  )
}
