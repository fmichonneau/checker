##' @importFrom dplyr %>%
##' @importFrom rlang .data

##' @importFrom dplyr case_when
uri_type <- function(scheme, server, ...) {
  dplyr::case_when(
    scheme == "data" ~ "data",
    scheme == "mailto" ~ "mailto",
    scheme == "" & server == "" ~ "local",
    TRUE ~ "external"
  )
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
    uri_type = uri_type(.data$scheme, .data$server),
    full_path = dplyr::case_when(
      ## within document urls
      uri_type == "local" & substr(.data$link, 1, 1) == "#" ~ doc,
      ## local files
      uri_type == "local" ~ file.path(base_path, .data$path),
      ## generic scheme (e.g. '//somewebsite.com')
      scheme == "" ~ paste0("https:", .data$link),
      ## data URI
      scheme == "data" ~ "<data URI>",
      ## other links
      TRUE ~ .data$link
    ),
    link = dplyr::case_when(
      uri_type == "data" ~ substr(link, 1, 100),
      TRUE ~ link
    )
  ) %>%
  ## remove empty links
  dplyr::filter(.data$link != "#")

  res
}

##' @importFrom fs file_exists
##' @importFrom purrr map_df
check_local_file <- function(full_path) {
  fs::file_exists(full_path) %>%
    purrr::map_df(function(.x) {
      if (!.x) {
        msg  <- "File referenced by URL doesn't exist."
      } else {
        msg <- "File exists."
      }

      list(
        valid = .x,
        message = msg
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

  results <- vector("list", length(full_path))
  chkr_pool <- curl::new_pool(total_con = length(full_path), host_con = 6,
                              multiplex = TRUE)


  for (i in seq_along(full_path)) {
    h <- curl::new_handle(url = full_path[i])

    success <- function() {
      orig_url <- full_path[i]
      idx <- i
      function(x) {
        p$tick()
        results[[idx]] <<-
          c(
            original_url = orig_url,
            x
          )
      }
    }

    failure <- function() {
      orig_url <- full_path[i]
      idx <- i
      function(str) {
        p$tick()
        results[[idx]] <<-
          c(
            original_url = orig_url,
            message = paste("Failed request: ", str)
          )
      }
    }

    curl::handle_setopt(h, nobody = 1L,
                        connecttimeout = 10L,
                        timeout = 15L,
                        failonerror = FALSE)
    curl::multi_add(h, done = success(), fail = failure(),
                    pool = chkr_pool)
  }
  curl::multi_run(pool = chkr_pool)

  results
}


##' @importFrom purrr map_df
check_url <- function(full_path) {

  check_url_raw(full_path) %>%
    purrr::map_df(
      function(.x) {
        if (exists("status_code", .x)) {
          list(
            url = .x$original_url,
            valid = .x$status_code == 200L,
            message = paste("HTTP status code:", .x$status_code))
        } else {
          list(
            url = .x$original_url,
            valid = FALSE,
            message = .x$message
          )
        }
      }
    )

}

no_check <- function(...) {
  tibble::tibble(
    valid = NA,
    message = ""
  )
}


##' @title Check links in your documents
##'
##' Currently only HTML files are supported.
##'
##' @details Data URI and \code{mailto:} links are not checked.
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

  uniq_links <- dplyr::distinct(links, .data$uri_type, .data$full_path)

  res <- uniq_links %>%
    dplyr::group_by(.data$uri_type) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fn = dplyr::case_when(
        uri_type == "local" ~ "check_local_file",
        uri_type == "external" ~ "check_url",
        uri_type %in% c("mailto", "data") ~ "no_check",
        TRUE ~ "stop"
      )) %>%
    dplyr::mutate(
      res = purrr::invoke_map(.data$fn, .data$data)
    )  %>%
    tidyr::unnest()

  out <- dplyr::left_join(links, res, by = c("full_path", "uri_type"))

  out <- out %>%
    check_fragments() %>%
    dplyr::select(.data$file, .data$link,
                  .data$full_path, .data$valid, .data$message)

  if (only_broken) {
    out <- out %>%
      dplyr::filter(!.data$valid)
  }

  summary_check_links(out)

  handle_raise(out, raise)

  invisible(out)

}

check_fragments_raw <- function(.dt, ...) {

  purrr::pmap(.dt, function(full_path, fragment, data, ...) {

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
  .d <- .d %>%
    tidyr::nest(.data$valid, .data$message)

  .d_res <- check_fragments_raw(.d)

  .d %>%
    dplyr::mutate(
      data = .d_res
    ) %>%
    tidyr::unnest()
}


##' @importFrom crayon red green
##' @importFrom cli symbol
summary_check_links <- function(.dt) {

  n_broken <- sum(!.dt$valid)
  n_valid <- sum(.dt$valid)

  if (n_valid == nrow(.dt)) {
    generic_msg(msg = "No broken links found.\n",
                type = "ok")
    return(.dt)
  }

  .dt <- .dt %>%
    dplyr::filter(!.data$valid)

  split(.dt, .dt$file) %>%
    generic_msg(
      msg = paste(n_broken, " broken links found\n"),
      type = "error"
    ) %>%
    purrr::walk(
      function(.x) {
        cat(
          crayon::green(
            paste("  ", cli::symbol$bullet, " in `",
                  crayon::underline(unique(.x$file)), "`\n",
                  sep = "")))
        purrr::pwalk(.x,
                     function(file, link, full_path, message, ...) {
                       cat(paste("    - link:", link,
                                 "\n      message: ", message, "\n"))
                     })
      }
    )

  .dt
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
