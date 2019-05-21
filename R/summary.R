generic_msg <- function(.dt = NULL,
                        msg,
                        type = c(
                          "success", "ok", "message", "warning", "error"
                        ),
                        indent = 0, raise = FALSE,
                        prefix = NULL,
                        show_icon = TRUE, ...) {

  type <- match.arg(type)
  if (is.null(prefix)) {
    prefix <- character(0)
  }

  if (show_icon) {
    icon <- switch(
      type,
      "success" = cli::symbol$tick,
      "ok" = character(0),
      "message" = cli::symbol$star,
      "warning" = cli::symbol$warning,
      "error" = cli::symbol$cross
    )
  } else {
    icon <- character(0)
  }

  col <- switch(
    type,
    "success" = crayon::green,
    "ok" = cat,
    "message" = crayon::blue,
    "warning" = crayon::yellow,
    "error" = crayon::red
  )

  if (raise) {
    f <- switch(
      type,
      "success" = "cat",
      "ok" = "cat",
      "message" = "message",
      "warning" = "warning",
      "error" = "stop"
    )
  } else {
    f  <- "cat"
  }

  cat_args <- list(
    col(
      crayon::bold(
        paste0(
          prefix,
          paste(rep(" ", indent), collapse = ""),
          icon, " ", msg
        ))
    )
  )

  if (identical(f, "stop")) {
    extra_args <- c(call. = list(FALSE))
  } else if (identical(f, "warning")) {
    extra_args <- c(
      call. = list(FALSE),
      immediate. = list(TRUE)
    )
  } else {
    extra_args <- NULL
  }

  cat_args <- c(cat_args, extra_args)

  rlang::exec(f, !!!cat_args)
  invisible(.dt)

}


##' @importFrom purrr iwalk
summary_check_links <- function(.dt, by) {

  n_valid <- get_n_success(.dt)

  ## only valid links, we can stop early
  ## NOTE: this might be problematic for links flagged "ok".
  if (identical(nrow(.dt), n_valid)) {
    generic_msg(
      msg = "No broken links found.\n",
      type = "success"
    )
    return(.dt)
  }


  split_dt <- split(.dt, .dt$error_level)

  possible_error_level <- c("success", checker_valid_options())

  split_dt <- split_dt[intersect(possible_error_level, names(split_dt))]

  purrr::iwalk(
    split_dt,
    function(.x, .y) {
      internal_summary_check_links(.dt = .x, type = .y, by = by)
    }
  )

  invisible(.dt)
}




##' @importFrom cli symbol
##' @importFrom purrr keep
##' @importFrom dplyr filter
internal_summary_check_links <- function(.dt, type, by) {

  out <- switch(by,
    page = split(.dt, .dt$file),
    resource = split(.dt,
      list(.dt$link, .dt$message)) %>%
      purrr::keep(~ nrow(.) > 0)
  )

  display <- switch(by,
    page = page_output,
    resource = resource_output
  )

  out %>%
      generic_msg(
        msg = paste(nrow(.dt), sQuote(type), "found:\n"),
        type = type
      ) %>%
    display()

  invisible(.dt)
}


##' @importFrom purrr walk pwalk
##' @importFrom crayon cyan underline
##' @importFrom cli symbol
page_output <- function(.dt) {
  .dt %>%
    purrr::walk(
      function(.x) {
        cat(
          crayon::cyan(
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

##' @importFrom purrr walk
##' @importFrom crayon cyan underline
##' @importFrom cli symbol
resource_output <- function(.dt) {
  .dt %>%
    purrr::walk(
      function(.x) {
        .rsrc <- unique(.x$link)
        .msg <- unique(.x$message)
        cat(
          crayon::cyan(
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


summary_check_images <- function(.dt) {

  .dt_img <- .dt[.dt$tag_type == "img", ]

  if (identical(nrow(.dt_img), 0L))
    return(invisible(.dt))

  process_alt_text(.dt_img)

  process_http_img(.dt_img)

  invisible(.dt)
}
