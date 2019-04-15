##' @importFrom xml2 read_html xml_find_all xml_text url_parse
##' @importFrom tibble tibble
##' @importFrom dplyr  bind_cols mutate
extract_links_html  <- function(doc, root_dir) {

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
      ## do a second pass on scheme, as broken self-contained images don't get
      ## parsed properly
      scheme = dplyr::case_when(
        !nzchar(.data$scheme) & grepl("^data\\:", .data$link) ~ "data",
        TRUE ~ .data$scheme
      ),
      uri_type = get_uri_type(.data$scheme, .data$server),
      full_path = dplyr::case_when(
        ## data URI
        scheme == "data" ~ convert_data_uri(.data$link),
        ## within document urls
        scheme == "" & uri_type == "local" & substr(.data$link, 1, 1) == "#" ~ doc,
        ## local files absolute paths
        scheme == "" & uri_type == "local" & substr(.data$link, 1, 1) == "/" ~ normalizePath(file.path(root_dir, .data$path), mustWork = FALSE),
        ## local files other types of paths
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

  ## Do a second pass: we modified some paths and URLs doing this allows to make
  ## sure we have more accurate data for the URI type.
  res <- res %>%
    ## we want to keep the original values for the bits that can from the
    ## original URLS as they won't be included in the `full_path` variable.
    ## For now, we only use `fragment`
    dplyr::select(
      -.data$scheme, -.data$server
    ) %>%
    dplyr::bind_cols(
      xml2::url_parse(res$full_path) %>%
        dplyr::select(-.data$fragment,
          -.data$port, -.data$user,
          -.data$path, -.data$query)
    )  %>%
    dplyr::mutate(
      uri_type = get_uri_type(.data$scheme, .data$server)
    )

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
  chkr_pool <- curl::new_pool(
    total_con = length(full_path),
    host_con = 12,
    multiplex = TRUE
  )


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
      connecttimeout = 15L,
      timeout = 30L,
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

check_data <- function(full_path, ...) {
  tibble::tibble(
    url = full_path,
    valid = !grepl("^data:text/html", full_path),
    message = dplyr::if_else(
      .data$valid,
      "",
      "Contained data represented as text, usually indicates incorrect path."
    )
  )
}

no_check <- function(full_path, ...) {
  tibble::tibble(
    url = full_path,
    valid = NA,
    message = ""
  )
}

unknown_protocol <- function(full_path, ...) {
  warning("Unknown protocol, for ", sQuote(full_path),
    " please report the issue: ",
    "https://github.com/fmichonneau/checker/issues/new")
}

extract_all_links <- function(dir, recursive, regexp, glob, root_dir, ...) {

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

  purrr::map_df(list_files, extract_links_html, root_dir, .id = "file")

}
