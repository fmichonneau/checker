##' @importFrom dplyr %>%
##' @importFrom rlang .data

##' @importFrom dplyr case_when
get_uri_type <- function(scheme, server, ...) {
  dplyr::case_when(
    scheme == "data" ~ "data",
    scheme == "mailto" ~ "mailto",
    scheme == "" & server == "" ~ "local",
    TRUE ~ "external"
  )
}


##' @importFrom xml2 read_html xml_find_all xml_text url_parse
##' @importFrom tibble tibble
##' @importFrom dplyr  bind_cols mutate
extract_links_html  <- function(doc) {

  doc <- normalizePath(doc)

  base_path <- dirname(doc)

  ## find all tags that have "href" or "src" attribute
  all_links <- xml2::read_html(doc) %>%
    xml2::xml_find_all(".//*[@href] | .//*[@src]")

  ## extract the tag type
  tag_type <- all_links %>%
    xml2::xml_name() %>%
    tolower()

  ## extract the target for the "href" or "src" attribute
  link_targets <- all_links %>%
    xml2::xml_find_all(".//@href | .//@src") %>%
    xml2::xml_text()

  ## extract the text (if applicable) that is marked up
  link_text <- all_links %>%
    xml2::xml_text()

  ## extract the alt text (only applicable to images)
  alt_text <- all_links %>%
    purrr::map_chr(function(.x) {
      .r <- xml2::xml_find_all(.x, ".//@alt") %>%
        xml2::xml_text()

      if (identical(length(.r), 0L))
        return(NA_character_)

      .r
    })

  ## assemble the result
  tbl_links <- tibble::tibble(
    tag_type = tag_type,
    link = link_targets,
    link_text = link_text,
    alt_text = alt_text
  ) %>%
    dplyr::distinct(.data$tag_type, .data$link, .data$link_text, .data$alt_text)

  res <- tbl_links %>%
    dplyr::bind_cols(
      xml2::url_parse(tbl_links$link)
    ) %>%
    dplyr::mutate(
      uri_type = get_uri_type(.data$scheme, .data$server),
      full_path = dplyr::case_when(
        ## data URI
        scheme == "data" ~ convert_data_uri(.data$link),
        ## within document urls
        scheme == "" & uri_type == "local" & substr(.data$link, 1, 1) == "#" ~ doc,
        ## local files
        scheme == "" & uri_type == "local" ~ normalizePath(file.path(base_path, .data$path), mustWork = FALSE),
        ## generic scheme (e.g. '//somewebsite.com')
        scheme == "" & nzchar(server) ~ paste0("https:", .data$link),
        ## other links
        TRUE ~ .data$link
      ),
      link = dplyr::case_when(
        uri_type == "data" ~ convert_data_uri(.data$link),
        TRUE ~ .data$link
      )
    ) %>%
    ## remove empty links
    dplyr::filter(.data$link != "#")

  res
}

##' @importFrom fs file_exists
##' @importFrom purrr map_df
check_local_file <- function(full_path) {
  purrr::map2_df(
    fs::file_exists(full_path),
    full_path,
    function(.x, .y) {
      if (!.x) {
        msg  <- "File referenced by URL doesn't exist."
      } else {
        msg <- "File exists."
      }

      list(
        url = .y,
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
          list(
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
check_url <- function(full_path, ...) {

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

no_check <- function(full_path, ...) {
  tibble::tibble(
    url = full_path,
    valid = NA,
    message = ""
  )
}


extract_all_links <- function(dir, recursive, regexp, glob, ...) {

  list_files <- fs::dir_ls(
    path = dir,
    recursive = recursive,
    regexp = regexp,
    glob = glob,
    ...
  )

  if (identical(length(list_files), 0L)) {
    warning("No files match your search.")
  }

  purrr::map_df(list_files, extract_links_html, .id = "file")

}

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
        uri_type %in% c("mailto", "data") ~ "no_check",
        TRUE ~ "stop"
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

    if (identical(uri_type, "external")) {
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


##' @importFrom crayon red blue
##' @importFrom cli symbol
summary_check_links <- function(.dt, by) {

  n_broken <- get_n_broken(.dt)
  n_valid <- get_n_valid(.dt)

  if (identical(n_valid, nrow(.dt))) {
    generic_msg(msg = "No broken links found.\n",
      type = "ok")
    return(.dt)
  }

  page_output <- function(x) {
    x %>%
      purrr::walk(
        function(.x) {
          cat(
            crayon::blue(
              paste("  ", cli::symbol$bullet, " in `",
                crayon::underline(unique(.x$file)), "`\n",
                sep = "")))
          purrr::pwalk(.x,
            function(file, link, link_text, full_path, message, ...) {
              if (nchar(link_text) > 0) {
                txt <- paste0("      text: ", dQuote(link_text), "\n")
              } else {
                txt <- character(0)
              }
              cat(paste0(
                "    - link: `", link, "`\n",
                txt,
                "      message: ", sQuote(message), "\n"))
            })
        }
      )
  }

  resource_output <- function(x) {
    x %>%
      purrr::walk(
        function(.x) {
          .rsrc <- unique(.x$link)
          .msg <- unique(.x$message)
          cat(
            crayon::blue(
              paste0("  ", cli::symbol$bullet,
                " Resource: `", crayon::underline(.rsrc), "`\n",
                "    Message: ", sQuote(.msg), "\n")
            ),
            sep = ""
          )
          cat("    Found in:\n")
          cat(
            paste0(
              "    - ", .x$file, "\n"
            ),
            sep = ""
          )
        }
      )
  }

  .dt_broken <- .dt %>%
    dplyr::filter(!.data$valid)

  out <- switch(by,
    page = split(.dt_broken, .dt_broken$file),
    resource = split(.dt_broken,
      list(.dt_broken$link, .dt_broken$message)) %>%
      purrr::keep(~ nrow(.) > 0)
  )

  display <- switch(by,
    page = page_output,
    resource = resource_output
  )

  out %>%
    generic_msg(
      msg = paste(n_broken, "broken links found:\n"),
      type = "error"
    ) %>%
    display

  invisible(.dt)
}

handle_raise <- function(out, raise) {

  msg <- "Broken links found."

  if (get_n_broken(out) > 0) {
    switch(raise,
           ok = NULL,
           warning = warning(msg, call. = FALSE),
           error = stop(msg, call. = FALSE))
  }

}

