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
      scheme == "" ~ file.path("https://", path),
      ## regular links
      TRUE ~ link
    )
  )

  res
}


check_local_file <- function(full_path) {
  fs::file_exists(full_path)
}


check_url_raw <- function(full_path) {

  results <- list()
  success <- function(x){
    results <<- append(results, list(x))
  }
  failure <- function(str){
    res <- paste("Failed request:", str)
    results <<- append(results, list(res))
  }

  for (i in seq_along(full_path)) {
    h <- curl::new_handle(url = full_path[i])
    curl::handle_setopt(h, nobody = 1L)
    curl::multi_add(h, done = success, fail = failure)
  }
  curl::multi_run()

  results
}

check_url <- function(url_chk) {

  .res <- check_url_raw(url_chk)

  url_chk <- purrr::map_df(
    .res,
    function(.x) {
      if (is.list(.x) && exists("status_code", .x))
        list(status_code = as.character(.x$status_code))
      else
        list(status_code = .x)
    })


  url_chk

}


as_param_list <- function(.x) {
  list(full_path = list(.x))
}


check_links <- function(dir = ".", recursive = TRUE, pattern = "html?$") {

  links <- fs::dir_ls(path = dir, recursive = recursive, regexp = pattern) %>%
    purrr::map_df(extract_links_html, .id = "file")

  uniq_links <- dplyr::distinct(links, is_local, full_path)

  res <- uniq_links %>%
    dplyr::group_by(is_local) %>%
    dplyr::summarize(pth = as_param_list(full_path)) %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        is_local ~ "check_local_file",
        !is_local ~ "check_url",
        TRUE ~ "stop"
      )) %>%
    dplyr::mutate(
      res = purrr::invoke_map(fn, pth)
    )

}
