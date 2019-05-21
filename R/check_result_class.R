new_check_result <- function(url, status_code, message, type, checker_options,
                             ...) {

  stopifnot(is.character(url))
  stopifnot(is.character(message))

  error_level <- error_level_url(status_code, message, type,
    checker_options = checker_options, ...)

  res <- tibble::tibble(
    url = url,
    error_level = error_level,
    message = message
  )
  class(res) <- c("url_check_results", class(res))

  res
}

url_check_result <- function(.x, type, checker_options, ...) {

  if (exists("status_code", .x)) {
    new_check_result(
      url = .x$original_url,
      status_code = .x$status_code,
      message = paste("HTTP status code:", .x$status_code),
      type = type,
      checker_options = checker_options,
      ...
    )
  } else {
    new_check_result(
      url = .x$original_url,
      status_code = NULL,
      message = .x$message,
      type = type,
      checker_options = checker_options,
      ...
    )
  }
}

error_level_url <- function(status_code = NULL, message, type, checker_options, ...) {

  if (identical(status_code, 200L))
    return(is_success())

  opts <- checker_options(checker_options = checker_options, ...)
  code_var <- paste0("status_code_", status_code)

  ## when dealing with localhost, the 404s returned by the server are actually
  ## local files, so we override the 404 option with the one provided by
  ## missing_files.
  if (identical(type, "localhost")) {
    opts[["status_code_404"]] <- opts[["missing_files"]]
  }

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
    TRUE ~ is_error()
  )

}
