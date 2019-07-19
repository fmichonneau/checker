check_fragments_raw <- function(.dt, checker_options, ...) {

  purrr::pmap(.dt, function(full_path, fragment, data, uri_type, ...) {

    if (!nzchar(fragment)) return(data)

    if (identical(uri_type, "local") && !fs::file_exists(full_path)) {
      opt_missing_file <- checker_options(checker_options)[["missing_local_file"]]
      return(
        tibble::tibble(
          error_level = opt_missing_file,
          message = sprintf("Local URL '%s' doesn't exist.",
            full_path)
        )
      )
    }

    ## file_exists above checks for existence of file. If the path is a
    ## directory (e.g., in the case of a site that runs on a local server such
    ## as Jekyll), then we append a final "/" so the server will return the
    ## correct path instead of trying to parse an empty document (which I think
    ## is an xml2 bug).
    if (fs::is_dir(full_path)) {
      full_path <- paste0(full_path, "/")
    }

    if (identical(uri_type, "local")) {
      doc_xml <- xml2::read_html(full_path, encoding = "utf-8")
    }

    if (identical(uri_type, "external") || identical(uri_type, "localhost")) {
      doc_xml <- try(xml2::read_html(full_path, encoding = "utf-8"),
        silent = TRUE)
      if (inherits(doc_xml, "try-error")) {
        return(
          tibble::tibble(
            error_level = checker_options(
              checker_options
            )[["html_parsing_failure"]],
            message = sprintf("Couldn't parse '%s': %s",
              full_path, doc_xml)
          )
        )
      }
    }


    test_string <- sprintf(".//*[@name=\"%s\"] | .//*[@id=\"%s\"]",
      fragment, fragment)

    res_anchor  <- doc_xml %>%
      xml2::xml_find_all(test_string) %>%
      length()

    if (res_anchor > 0L) {
      res <- list(
        error_level = is_success(),
        message = sprintf("Fragment ('%s') checked and found.", fragment)
      )
    } else {
      opt_anchor <- checker_options(checker_options)[["missing_anchor"]]
      res <- list(
        error_level = opt_anchor,
        message = sprintf(
          "URL is valid but fragment (hash reference): '%s' not found in page.",
          fragment
        )
      )
    }
    tibble::as_tibble(res)
  })

}

check_fragments <- function(.d, checker_options, ...) {
  .d <- .d %>%
    tidyr::nest(.data$error_level, .data$message)

  .d_res <- check_fragments_raw(.d, checker_options)

  .d %>%
    dplyr::mutate(
      data = .d_res
    ) %>%
    tidyr::unnest()
}
