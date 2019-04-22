## All the options can take the values: 0, 1, 2, 3
## - 0: all good, nothing to see
## - 1: note/message level.
## - 2: warning level.
## - 3: error level.
checker_default_options <- function() {
  list(
    ## files
    missing_files = 3L,
    ## anchors
    missing_anchor = 3L,
    ## misc
    ssl_timeout = 1L,
    operation_timeout = 1L,
    connection_timeout = 1L,
    resolve_timeout = 1L,
    fail_resolve = 2L,
    ## 300
    status_code_300 = 2L,
    status_code_301 = 2L,
    status_code_302 = 2L,
    status_code_303 = 2L,
    status_code_304 = 2L,
    status_code_305 = 2L,
    status_code_306 = 2L,
    status_code_307 = 2L,
    status_code_308 = 2L,
    ## 400
    status_code_400 = 2L,
    status_code_401 = 2L,
    status_code_402 = 2L,
    status_code_403 = 2L,
    status_code_404 = 3L, ## <- 404 is here
    status_code_405 = 2L,
    status_code_406 = 2L,
    status_code_407 = 2L,
    status_code_408 = 2L,
    status_code_409 = 2L,
    status_code_410 = 2L,
    status_code_411 = 2L,
    status_code_412 = 2L,
    status_code_413 = 2L,
    status_code_414 = 2L,
    status_code_415 = 2L,
    status_code_416 = 2L,
    status_code_417 = 2L,
    status_code_418 = 2L,
    status_code_421 = 2L,
    status_code_422 = 2L,
    status_code_423 = 2L,
    status_code_424 = 2L,
    status_code_425 = 2L,
    status_code_426 = 2L,
    status_code_428 = 2L,
    status_code_429 = 2L,
    status_code_431 = 2L,
    status_code_451 = 2L,
    ## 500
    status_code_500 = 2L,
    status_code_501 = 2L,
    status_code_502 = 2L,
    status_code_503 = 2L,
    status_code_504 = 2L,
    status_code_505 = 2L,
    status_code_506 = 2L,
    status_code_507 = 2L,
    status_code_508 = 2L,
    status_code_510 = 2L,
    status_code_511 = 2L
  )
}

##' Checker Options
##'
##' @details Customize which HTTP status codes and other situations lead checker
##'   to mark links and other URIs as problematic.
##'
##' Each options can take one of 4 values: 0, 1, 2, or 3:
##' - 0: silently ignored situation
##' - 1: message-level notification that the situation was encountered
##' - 2: warning-level notification
##' - 3: error-level notification.
##'
##' For instance, you will most likely always want `status_code_404 = 3L`, so
##' checker will report as invalid 404 HTTP status codes.
##'
##' You can customize default values by:
##'
##' * specifying options in your .Rprofile
##'
##' * passing a list of the options you want to customize to `check_links()`
##' using the `checker_options` argument
##'
##' @param checker_options a named list of the checker options to customize.
##' @param ... ignored for now
##' @return a customized list of checker options
##' @examples
##' ## check_links(..., checker_options = list(status_code_300 = 0L, status_code_301 = 2L))
##' @importFrom utils modifyList
##' @importFrom purrr map_lgl map possibly
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

  safe_as_int <- purrr::possibly(as.integer, otherwise = NA_integer_)

  usr <- usr[names(usr) %in% names(def)]
  usr <- suppressWarnings(purrr::map(usr, safe_as_int))
  res <- utils::modifyList(def, usr)

  if (!all(purrr::map_lgl(res, is.integer)) ||
        any(is.na(res)))
    stop("all checker_options values must be integers (0, 1, 2 or 3).",
      call. = FALSE)

  res
}
