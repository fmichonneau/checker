
extract_img_html <- function(doc) {

  doc <- normalizePath(doc)
  base_path <- dirname(doc)

  all_imgs <- xml2::read_html(doc) %>%
    xml2::xml_find_all(".//img")

  srcs <- purrr::map(all_imgs,
                     ~ xml2::xml_find_all(., "@src") %>%
                       xml2::xml_text()) %>%
    purrr::map_chr(function(.x) {
      if (length(.x) == 0L) return(NA_character_)
      if (grepl("^data:", .x)) return("<data URI>")
      .x
    })

  alt_txt <- purrr::map(all_imgs,
                        ~ xml2::xml_find_all(., "@alt") %>%
                          xml2::xml_text()
                        ) %>%
    purrr::map_chr(function(.x) {
      if (length(.x) == 0L) return(NA_character_)
      .x
    })


  tibble::tibble(
    src = srcs,
    alt = alt_txt
  )

}

check_images <- function(dir = ".", recursive = TRUE,
                         regexp = "\\.html?$", glob = NULL,
                         show_full_path = FALSE,
                         ...) {

  images <- fs::dir_ls(
    path = dir,
    recursive = recursive,
    regexp = regexp,
    glob = glob,
    ...
  ) %>%
    purrr::map_df(extract_img_html, .id = "file") %>%
  make_path_rel(dir = dir, show_full_path = show_full_path)

  images

}

summary_check_images <- function(.dt) {

  if (nrow(.dt) == 0L || sum(is.na(.dt$alt)) == 0L) {
    generic_msg(msg = "All images passed the checks.\n", type = "ok")
    return(.dt)
  }

  .dt %>%
    dplyr::filter(is.na(.data$alt)) %>%
    split(.data$file) %>%
    generic_msg(
      msg = "No 'alt' text for the following images:\n",
      type = "warning"
    ) %>%
    purrr::walk(
      function(.x) {
        cat(
          crayon::green(
            paste("  ", cli::symbol$bullet, " in `",
                  crayon::underline(unique(.x$file)), "`\n",
                  sep = "")))
        purrr::pwalk(.x,
                     function(file, src, alt, ...) {
                       cat(paste("    ", src, "\n"))
                     })
      }
    )

  .dt
}
