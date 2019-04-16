checker_default_options <- function() {
  list(
    ssl_timeout = FALSE,
    operation_timeout = FALSE,
    connection_timeout = FALSE,
    resolve_timeout = FALSE,
    fail_resolve = TRUE,
    ## 300
    status_code_300 = FALSE,
    status_code_301 = FALSE,
    status_code_302 = FALSE,
    status_code_303 = FALSE,
    status_code_304 = FALSE,
    status_code_305 = FALSE,
    status_code_306 = FALSE,
    status_code_307 = FALSE,
    status_code_308 = FALSE,
    ## 400
    status_code_400 = TRUE,
    status_code_401 = TRUE,
    status_code_402 = TRUE,
    status_code_403 = TRUE,
    status_code_404 = TRUE,
    status_code_405 = TRUE,
    status_code_406 = TRUE,
    status_code_407 = TRUE,
    status_code_408 = TRUE,
    status_code_409 = TRUE,
    status_code_410 = TRUE,
    status_code_411 = TRUE,
    status_code_412 = TRUE,
    status_code_413 = TRUE,
    status_code_414 = TRUE,
    status_code_415 = TRUE,
    status_code_416 = TRUE,
    status_code_417 = TRUE,
    status_code_418 = TRUE,
    status_code_421 = TRUE,
    status_code_422 = TRUE,
    status_code_423 = TRUE,
    status_code_424 = TRUE,
    status_code_425 = TRUE,
    status_code_426 = TRUE,
    status_code_428 = TRUE,
    status_code_429 = TRUE,
    status_code_431 = TRUE,
    status_code_451 = TRUE,
    ## 500
    status_code_500 = TRUE,
    status_code_501 = TRUE,
    status_code_502 = TRUE,
    status_code_503 = TRUE,
    status_code_504 = TRUE,
    status_code_505 = TRUE,
    status_code_506 = TRUE,
    status_code_507 = TRUE,
    status_code_508 = TRUE,
    status_code_510 = TRUE,
    status_code_511 = TRUE
  )
}

##' Checker Options
##'
##' @details Customize which HTTP status codes and other situations lead checker
##'   to mark links and other URIs as invalid/broken.
##'
##' You can customize default values by:
##'
##' * specifying options in your .Rprofile
##'
##' * passing a list of the options you want to customize to `check_link()`
##' using the `checker_options` argument
##'
##' @param checker_options a named list of the checker options to customize.
##' @param ... ignored
##' @return a customized list of checker options
##' @examples
##' ## check_links(..., checker_options = list(status_code_300 = TRUE, status_code_301 = TRUE))
##' @importFrom utils modifyList
##' @importFrom purrr map_lgl
##' @export
checker_options <- function(checker_options, ...) {
  def <- checker_default_options()

  if (missing(checker_options)) {
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
  res <- utils::modifyList(def, usr)

  if (!all(purrr::map_lgl(res, is.logical)) ||
        any(is.na(res)))
    stop("all checker_options values must be TRUE or FALSE")

  res
}
