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
  )

  dplyr::bind_cols(
    res,
    xml2::url_parse(links)
  ) %>%
    dplyr::mutate(
      is_local = is_local(scheme, server),
      full_path = case_when(
        ## local files
        is_local ~ file.path(base_path, path),
        ## generic scheme (e.g. '//somewebsite.com')
        scheme == "" ~ file.path("https://", path),
        ## regular links
        TRUE ~ link
      )
    )

}


check_local_file <- function(pth) {
  fs::file_exists(pth)
}


check_url <- function(pth) {

  results <- list()
  success <- function(x){
    results <<- append(results, list(x))
  }
  failure <- function(str){
    cat(paste("Failed request:", str), file = stderr())
  }

  for (i in seq_along(pth)) {
    h <- curl::new_handle(url = pth[i])
    curl::handle_setopt(h, nobody = 1L)
    curl::multi_add(h, done = success,
                    ## TODO figure how to deal with failure gracefully
                    fail = success)
  }
  multi_run()

  results
}
