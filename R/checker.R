##' @importFrom dplyr %>%
##' @importFrom rlang .data

##' @importFrom purrr map2_lgl
is_local <- function(scheme, server, ...) {
  ## local files
  purrr::map2_lgl(scheme, server, ~ .x == "" & .y == "")
}


##' @importFrom xml2 read_html xml_find_all xml_text url_parse
##' @importFrom tibble data_frame
##' @importFrom dplyr  bind_cols mutate
extract_links_html  <- function(doc) {

  doc <- normalizePath(doc)

  base_path <- dirname(doc)

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
    is_local = is_local(.data$scheme, .data$server),
    full_path = dplyr::case_when(
      ## within document urls
      is_local & substr(.data$link, 1, 1) == "#" ~ doc,
      ## local files
      is_local ~ file.path(base_path, .data$path),
      ## generic scheme (e.g. '//somewebsite.com')
      scheme == "" ~ paste0("https:", .data$link),
      ## regular links
      TRUE ~ .data$link
    )
  ) %>%
  ## remove empty links
  dplyr::filter(link != "#")

  res
}

##' @importFrom fs file_exists
##' @importFrom purrr map_df
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
##' @importFrom curl new_handle handle_setopt multi_add multi_run
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
    curl::handle_setopt(h, nobody = 1L,
                        connecttimeout = 5L,
                        timeout = 10L,
                        failonerror = FALSE)
    curl::multi_add(h, done = success, fail = failure)
  }
  curl::multi_run(timeout = 10L)

  results
}

##' @importFrom purrr map_df
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


##' @title Check links in your documents
##'
##' Currently only HTML files are supported
##'
##' @param dir The directory to look for documents
##' @param recursive Should sub-folders be searched for documents? (default
##'   `TRUE`).
##' @param regexp A regular expression matching the names of the files to check.
##' @param glob A wildcard pattern matching the names fo the files to check.
##' @param only_broken Should the results include only the broken links
##'   (default) or also the valid links?
##' @param raise If set to `warning` or `error`, the function will raise a
##'   warning or an error if broken links are found.
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
                        only_broken = TRUE,
                        raise = c("ok", "warning", "error"), ...) {

  raise <- match.arg(raise)

  links <- fs::dir_ls(
    path = dir,
    recursive = recursive,
    regexp = regexp,
    glob = glob,
    ...
  ) %>%
    purrr::map_df(extract_links_html, .id = "file")

  uniq_links <- dplyr::distinct(links, .data$is_local, .data$full_path)

  res <- uniq_links %>%
    dplyr::group_by(.data$is_local) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        is_local ~ "check_local_file",
        !is_local ~ "check_url",
        TRUE ~ "stop"
      )) %>%
    dplyr::mutate(
      res = purrr::invoke_map(.data$fn, .data$data)
    ) %>%
    tidyr::unnest()

  out <- dplyr::left_join(links, res, by = c("full_path", "is_local")) %>%
    check_fragments() %>%
    dplyr::select(.data$file, .data$link,
                  .data$full_path, .data$valid, .data$message)

  if (only_broken) {
    out <- out %>%
      dplyr::filter(!.data$valid)
  }

  summary_check_links(out)

  handle_raise(out, raise)

  out

}

check_fragments_raw <- function(.data) {

  purrr::pmap(.data, function(full_path, fragment, data, ...) {

    if (!nzchar(fragment)) return(data)

    doc_xml <- xml2::read_html(full_path, encoding = "utf-8")

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
  .d %>%
    tidyr::nest(valid, message) %>%
    dplyr::mutate(
      data = check_fragments_raw(.)
    ) %>%
    tidyr::unnest()
}


##' @importFrom crayon red green
##' @importFrom cli symbol
summary_check_links <- function(out) {

  n_broken <- sum(!out$valid)
  n_valid <- sum(out$valid)

  if (n_broken > 0) {
    cat(crayon::red(cli::symbol$cross,  n_broken, "links are broken.\n"))
  }

  if (n_valid > 0) {
    cat(crayon::green(cli::symbol$tick, n_valid, "links are valid.\n"))
  }

  if (n_valid == nrow(out)) {
    cat(crayon::green(cli::symbol$tick, " No broken links found.\n"))
  }

}


handle_raise <- function(out, raise) {

  msg <- "Broken links found."

  if (sum(!out$valid) > 0) {
    switch(raise,
           ok = NULL,
           warning = warning(msg, call. = FALSE),
           error = stop(msg, call. = FALSE))
  }

}
