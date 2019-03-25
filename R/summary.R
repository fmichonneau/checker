generic_msg <- function(.dt = NULL,
                        msg, type = c("note", "warning", "error", "ok"),
                        indent = 0, ...) {

  type <- match.arg(type)

  icon <- switch(type,
                 "note" = cli::symbol$star,
                 "warning" = cli::symbol$warning,
                 "error" = cli::symbol$cross,
                 "ok" = cli::symbol$tick)

  col <- switch(type,
                "note" = crayon::blue,
                "warning" = crayon::yellow,
                "error" = crayon::red,
                "ok" = crayon::green)

  cat(col(
    crayon::bold(
      paste(rep(" ", indent), collapse = ""),
      icon, msg
    )))

  invisible(.dt)

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
