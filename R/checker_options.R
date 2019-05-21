## All the options can take the values:
## - "ok": no output, silently ignored.
## - "message": note/message level.
## - "warning": warning level.
## - "error": error level.
checker_default_options <- function() {
  list(
    ## files
    missing_local_file = "error",
    ## broken local data URI
    broken_data_uri = "error",
    ## fail to parse HTML page to check whether it includes the specified anchor
    html_parsing_failure = "warning",
    ## anchors
    missing_anchor = "error",
    ## not checked (e.g., mailto:)
    not_checked = "ok",
    ## unsupported protocol
    unsupported_protocol = "warning",
    ## denied by robots
    robots_denied = "message",
    ## misc
    ssl_timeout = "message",
    operation_timeout = "message",
    connection_timeout = "message",
    resolve_timeout = "message",
    fail_resolve = "warning",
    ## 300
    status_code_300 = "warning",
    status_code_301 = "warning",
    status_code_302 = "warning",
    status_code_303 = "warning",
    status_code_304 = "warning",
    status_code_305 = "warning",
    status_code_306 = "warning",
    status_code_307 = "warning",
    status_code_308 = "warning",
    ## 400
    status_code_400 = "warning",
    status_code_401 = "warning",
    status_code_402 = "warning",
    status_code_403 = "warning",
    status_code_404 = "error",    ## <- 404 is here
    status_code_405 = "warning",
    status_code_406 = "warning",
    status_code_407 = "warning",
    status_code_408 = "warning",
    status_code_409 = "warning",
    status_code_410 = "warning",
    status_code_411 = "warning",
    status_code_412 = "warning",
    status_code_413 = "warning",
    status_code_414 = "warning",
    status_code_415 = "warning",
    status_code_416 = "warning",
    status_code_417 = "warning",
    status_code_418 = "warning",
    status_code_421 = "warning",
    status_code_422 = "warning",
    status_code_423 = "warning",
    status_code_424 = "warning",
    status_code_425 = "warning",
    status_code_426 = "warning",
    status_code_428 = "warning",
    status_code_429 = "warning",
    status_code_431 = "warning",
    status_code_451 = "warning",
    ## 500
    status_code_500 = "warning",
    status_code_501 = "warning",
    status_code_502 = "warning",
    status_code_503 = "warning",
    status_code_504 = "warning",
    status_code_505 = "warning",
    status_code_506 = "warning",
    status_code_507 = "warning",
    status_code_508 = "warning",
    status_code_510 = "warning",
    status_code_511 = "warning"
  )
}

checker_valid_options <- function() {

  c("ok", "message", "warning", "error")

}

checker_check_options <- function(opt) {

  if (is.null(opt))
    stop("null value")

  match.arg(opt, checker_valid_options())

}

##' Checker Options
##'
##' @details Customize which HTTP status codes and other situations lead checker
##'   to mark links and other URIs as problematic.
##'
##' Each options can take one of 4 values:
##' - "ok": silently ignored situation
##' - "message": message-level notification that the situation was encountered
##' - "warning": warning-level notification
##' - "error": error-level notification.
##'
##' For instance, you will most likely always want `status_code_404 = "error"`,
##' so checker will report as invalid 404 HTTP status codes.
##'
##' You can customize default values by:
##'
##' * specifying options in your `.Rprofile`
##'
##' * passing a list of the options you want to customize to `check_links()`
##' using the `checker_options` argument
##'
##' @param checker_options a named list of the checker options to customize.
##' @param ... ignored for now
##' @return a customized list of checker options
##' @examples
##' ## check_links(..., checker_options = list(status_code_300 = "ok", status_code_301 = "warning"))
##' @importFrom utils modifyList
##' @importFrom purrr map safely walk
##' @export
checker_options <- function(checker_options, ...) {
  def <- checker_default_options()

  if (is.null(checker_options)) {
    usr <- getOption("checker.options")
  } else {
    if (!is.list(checker_options))
      stop(sQuote("checker_options"), " must be a list.", call. = FALSE)
    usr <- checker_options
  }

  if (is.null(usr)) {
    return(def)
  }

  usr <- usr[names(usr) %in% names(def)]

  chk_usr <- map(usr, purrr::safely(checker_check_options))
  
  purrr::walk(chk_usr, function(.x) {
    if (!is.null(.x$error))
      stop("all ", sQuote("checker_options"), " values must be one of: ",
        paste(dQuote(checker_valid_options()), collapse = ", "), ".",
        call. = FALSE)
  })
  
  utils::modifyList(def, usr)
  
}
