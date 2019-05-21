assertion_factory <- function(level) {
  force(level)
  function(.dt) {
    .dt$error_level == level
  }
}

error_assertion <- assertion_factory("error")
warning_assertion <- assertion_factory("warning")
message_assertion <- assertion_factory("message")
ok_assertion <- assertion_factory("ok")
success_assertion <- assertion_factory("success")

get_n_factory <- function(level) {
  force(level)
  function(.dt) {
    sum(assertion_factory(level)(.dt))
  }
}

get_n_error <- get_n_factory("error")
get_n_warning <- get_n_factory("warning")
get_n_message <- get_n_factory("message")
get_n_ok <- get_n_factory("ok")
get_n_success <- get_n_factory("success")

has_issues_assertion <- function(.dt) {
  (!success_assertion(.dt)) |
    (.dt$tag_type == "img" & is.na(.dt$alt_text)) |
    (.dt$tag_type == "img" & .dt$scheme == "http")
}


handle_raise <- function(out, stop_on_error) {

  n_note <- get_n_message(out)
  n_warning <- get_n_warning(out)
  n_error <- get_n_error(out)

  tryCatch(
    purrr::walk2(
      c(n_note, n_warning, n_error),
      c("message", "warning", "error"),
      function(.x, .y) {
        if (.x > 0) {
          if (identical(.y, "message")) {
            prefix <- "Message: "
          } else {
            prefix <- character(0)
          }
          generic_msg(
            msg = paste0(
              .x, " links are generating ",
              .y, "-level situation."
            ),
            type = .y,
            raise = TRUE,
            prefix = prefix,
            show_icon = FALSE
          )
        }
      }
    ),
    error = function(e) {
      if (!stop_on_error) {
        message("Error: ", e$message)
        invisible(out)
      } else {
        stop(e$message, call. = FALSE)
      }
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )

}
