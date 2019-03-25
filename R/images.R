process_alt_text <- function(.dt_img) {

  if (identical(nrow(.dt_img), 0L)) {
    return(NULL)
  }

  .dt_img <- .dt_img %>%
    dplyr::filter(is.na(.data$alt_text))

  if (identical(sum(is.na(.dt_img$alt_text)), 0L)) {
    generic_msg(msg = "All images passed the alt-text checks.\n", type = "ok")
    return(NULL)
  }

  split(.dt_img, .dt_img$file) %>%
    generic_msg(
      msg = "No 'alt' text for the following images:\n",
      type = "warning"
    ) %>%
    purrr::walk(
      function(.x) {
        cat(
          crayon::blue(
            paste("  ", cli::symbol$bullet, " in `",
              crayon::underline(unique(.x$file)), "`\n",
              sep = "")))
        purrr::pwalk(.x,
          function(file, link, alt_text, ...) {
            cat(paste("   - ", link, "\n"))
          })
      }
    )
}


process_http_img <- function(.dt_img) {

  if (identical(nrow(.dt_img), 0L)) {
    return(NULL)
  }

  .dt_img_http <- .dt_img[.dt_img$scheme == "http", ]

  if (identical(nrow(.dt_img_http), 0L)) {
    generic_msg(msg = "All images passed the HTTP checks.\n", type = "ok")
    return(NULL)
  }

  split(.dt_img_http, .dt_img_http$file) %>%
    generic_msg(
      msg = "Using HTTP protocol for the following images. It's recommended to use HTTPS:\n",
      type = "warning"
    ) %>%
    purrr::walk(
      function(.x) {
        cat(
          crayon::blue(
            paste("  ", cli::symbol$bullet, " in `",
              crayon::underline(unique(.x$file)), "`\n",
              sep = "")))
        purrr::pwalk(.x,
          function(file, link, alt_text, ...) {
            cat(paste("   - ", link, "\n"))
          })
      }
    )
}


summary_check_images <- function(.dt) {

  .dt_img <- .dt[.dt$tag_type == "img", ]

  if (identical(nrow(.dt_img), 0L))
    return(invisible(.dt))

  process_alt_text(.dt_img)

  process_http_img(.dt_img)

  invisible(.dt)
}
