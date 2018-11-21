is_local <- function(scheme, server, ...) {
  ## local files
  purrr::map2_lgl(scheme, server, ~ .x == "" & .y == "")
}

extract_links_html  <- function(doc) {

  base_path <- dirname(normalizePath(doc))

  links <- xml2::read_html(doc) %>%
    xml2::xml_find_all(".//*[@href]/@href | .//*[@src]/@src") %>%
    xml2::xml_text() %>%
    unique()

  res <- tibble::data_frame(
    link = links
  ) %>%
    dplyr::bind_cols(
      xml2::url_parse(links)
    ) %>%
  dplyr::mutate(
    is_local = is_local(scheme, server),
    full_path = dplyr::case_when(
      ## local files
      is_local ~ file.path(base_path, path),
      ## generic scheme (e.g. '//somewebsite.com')
      scheme == "" ~ paste0("https:", link),
      ## regular links
      TRUE ~ link
    )
  )

  res
}


check_local_file <- function(full_path) {
  fs::file_exists(full_path) %>%
    purrr::map_df(function(.x) {
      list(
        valid = .x,
        message = NA_character_
      )
    })
}


##' @importFrom progress progress_bar
check_url_raw <- function(full_path) {

  p <- progress::progress_bar$new(
    total = length(full_path),
    format = "  Checking link :current out of :total [:bar] :percent"
  )

  results <- list()

  success <- function(x) {
    p$tick()
    results <<- append(results, list(x))
  }
  failure <- function(str) {
    p$tick()
    res <- paste("Failed request:", str)
    results <<- append(results, list(res))
  }

  for (i in seq_along(full_path)) {
    h <- curl::new_handle(url = full_path[i])
    curl::handle_setopt(h, nobody = 1L, connecttimeout = 200L,
                        failonerror = FALSE)
    curl::multi_add(h, done = success, fail = failure)
  }
  curl::multi_run(timeout = 10)

  results
}

check_url <- function(full_path) {

  check_url_raw(full_path) %>%
    purrr::map_df(
      function(.x) {
        if (is.list(.x) && exists("status_code", .x)) {
          list(
            valid = .x$status_code == 200L,
            message = paste("HTTP status code:", .x$status_code))
        } else {
          list(
            valid = FALSE,
            message = .x
          )
        }
      })

}


check_links <- function(dir = ".", recursive = TRUE,
                        regexp = "html?$", glob = NULL,
                        only_broken = TRUE, ...) {

  links <- fs::dir_ls(
    path = dir,
    recursive = recursive,
    regexp = regexp,
    glob = glob,
    ...
  ) %>%
    purrr::map_df(extract_links_html, .id = "file")

  uniq_links <- dplyr::distinct(links, is_local, full_path)

  res <- uniq_links %>%
    dplyr::group_by(is_local) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        is_local ~ "check_local_file",
        !is_local ~ "check_url",
        TRUE ~ "stop"
      )) %>%
    dplyr::mutate(
      res = purrr::invoke_map(fn, data)
    ) %>%
    tidyr::unnest()

  out <- dplyr::left_join(links, res, by = "full_path") %>%
    dplyr::select(file, link, full_path, valid, message)

  if (only_broken) {
    out <- out %>%
      dplyr::filter(!valid)
  }



  out

}



}
