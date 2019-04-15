new_check_result <- function(url, status_code, message, ...) {

  stopifnot(is.character(url))
  stopifnot(is.character(message))

  valid <- is_valid(status_code, message, ...)

  res <- tibble::tibble(
    url = url,
    valid = valid,
    message = message
  )
  class(res) <- c("url_check_results", class(res))

  res
}

url_check_result <- function(.x) {
  if (exists("status_code", .x)) {
    new_check_result(
      url = .x$original_url,
      status_code = .x$status_code,
      message = paste("HTTP status code:", .x$status_code))
  } else {
    new_check_result(
      url = .x$original_url,
      status_code = NULL,
      message = .x$message
    )
  }
}


is_valid <- function(status_code = NULL, message, opts) {

  if (identical(status_code, 200L))
    return(TRUE)

  opts <- checker_options(opts)
  code_var <- paste0("status_code_", status_code)
  if (exists(code_var, opts))
    return(opts[[code_var]])

  dplyr::case_when(
    grepl("ssl connection timeout", message, ignore.case = TRUE) ~
      opts[["ssl_timeout"]],
    grepl("operation timed out after", message, ignore.case = TRUE) ~
      opts[["operation_timeout"]],
    grepl("connection time-out", message, ignore.case = TRUE) ~
      opts[["connection_timeout"]],
    grepl("resolving timed out after", message, ignore.case = TRUE) ~
      opts[["resolve_timeout"]],
    grepl("could not resolve host", message, ignore.case = TRUE) ~
      opts[["fail_resolve"]],
    TRUE ~ FALSE
  )

}
