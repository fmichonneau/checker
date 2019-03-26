check_fragments_raw <- function(.dt, ...) {

  purrr::pmap(.dt, function(full_path, fragment, data, uri_type, ...) {

    if (!nzchar(fragment)) return(data)

    if (identical(uri_type, "local") && !fs::file_exists(full_path)) {
      return(
        tibble::tibble(
          valid = FALSE,
          message = sprintf("Local URL '%s' doesn't exist.",
            full_path)
        )
      )
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
            valid = NA,
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
        valid = TRUE,
        message = sprintf("Fragment ('%s') checked and found.", fragment)
      )
    } else {
      res <- list(
        valid = FALSE,
        message = sprintf(
          "URL is valid but fragment (hash reference): '%s' not found in page.",
          fragment
        )
      )
    }
    tibble::as_tibble(res)
  })

}

check_fragments <- function(.d, ...) {
  .d <- .d %>%
    tidyr::nest(.data$valid, .data$message)

  .d_res <- check_fragments_raw(.d)

  .d %>%
    dplyr::mutate(
      data = .d_res
    ) %>%
    tidyr::unnest()
}
