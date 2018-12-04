
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

make_path_rel <- function(.data, dir, show_full_path) {
  if (show_full_path)
    return(.data)

  dir <- normalizePath(dir)

  .data %>%
    dplyr::mutate(
      file = gsub(dir, ".", .data$file)
    )
}

check_images <- function(dir = ".", recursive = TRUE,
                         regexp = "html?$", glob = NULL,
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
