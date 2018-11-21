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
      scheme == "" ~ paste0("http", link),
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


  results <- list()
  success <- function(x){
    results <<- append(results, list(x))
  }
  failure <- function(str){
    res <- paste("Failed request:", str)
    results <<- append(results, list(res))
  }

  p <- progress_multi(length(full_path), labels = NULL, count = TRUE,
                      progress = TRUE)

  for (i in seq_along(full_path)) {
    h <- curl::new_handle(url = full_path[i], progressfunction = p$callback)
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


check_links <- function(dir = ".", recursive = TRUE, pattern = "html?$") {

  links <- fs::dir_ls(path = dir, recursive = recursive, regexp = pattern) %>%
    purrr::map_df(extract_links_html, .id = "file") %>%
    readr::write_csv("/tmp/res.csv")

  uniq_links <- dplyr::distinct(links, is_local, full_path)

  res <- uniq_links %>%
    dplyr::group_by(is_local) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        is_local ~ "check_local_file",
        !is_local ~ "check_url",
        TRUE ~ "stop"
      ))

  browser()
  res <- dplyr::mutate(res,
                       res = purrr::invoke_map(fn, data)
                       )

  res <- res %>%
    tidyr::unnest()

  dplyr::left_join(links, res, by = "full_path") %>%
    dplyr::select(file, link, full_path, valid, message)

}


progress_multi <- function(i, labels, count, progress) {
  label <- format(labels[[i]], width = max(nchar(labels)), justify = "right")
  if (count) {
    is <- format(i, width = nchar(length(labels)))
    prefix <- sprintf("[%s/%s] %s", is, length(labels), label)
  } else {
    prefix <- label
  }
  bar <- NULL
  type <- "down"
  seen <- 0

  if (progress) {
    callback <- function(down, up) {
      if (type == "down") {
        total <- down[[1L]]
        now <- down[[2L]]
      } else {
        total <- up[[1L]]
        now <- up[[2L]]
      }

      if (total == 0 && now == 0) {
        bar <<- NULL
        seen <<- 0
        return(TRUE)
      }

      if (is.null(bar)) {
        if (total == 0) {
          fmt <- paste0(prefix, " [ :bytes in :elapsed ]")
          total <- 1e8 # arbitrarily big
        } else {
          fmt <- paste0(prefix, " [:percent :bar]")
        }
        bar <<- progress::progress_bar$new(fmt, total, clear = TRUE,
                                           show_after = 0)
      }
      if (total == 0) {
        bar$tick(now)
      } else {
        bar$tick(now - seen)
        seen <<- now
      }

      TRUE
    }
  } else {
    callback <- function(down, up) {
      TRUE
    }
  }

  list(callback = callback,
       prefix = prefix)
}
