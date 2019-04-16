checker_default_options <- function() {
  list(
    ssl_timeout = TRUE,
    operation_timeout = TRUE,
    connection_timeout = TRUE,
    resolve_timeout = TRUE,
    fail_resolve = FALSE,
    ## 300
    status_code_300 = TRUE,
    status_code_301 = TRUE,
    status_code_302 = TRUE,
    status_code_303 = TRUE,
    status_code_304 = TRUE,
    status_code_305 = TRUE,
    status_code_306 = TRUE,
    status_code_307 = TRUE,
    status_code_308 = TRUE,
    ## 400
    status_code_400 = FALSE,
    status_code_401 = FALSE,
    status_code_402 = FALSE,
    status_code_403 = FALSE,
    status_code_404 = FALSE,
    status_code_405 = FALSE,
    status_code_406 = FALSE,
    status_code_407 = FALSE,
    status_code_408 = FALSE,
    status_code_409 = FALSE,
    status_code_410 = FALSE,
    status_code_411 = FALSE,
    status_code_412 = FALSE,
    status_code_413 = FALSE,
    status_code_414 = FALSE,
    status_code_415 = FALSE,
    status_code_416 = FALSE,
    status_code_417 = FALSE,
    status_code_418 = FALSE,
    status_code_421 = FALSE,
    status_code_422 = FALSE,
    status_code_423 = FALSE,
    status_code_424 = FALSE,
    status_code_425 = FALSE,
    status_code_426 = FALSE,
    status_code_428 = FALSE,
    status_code_429 = FALSE,
    status_code_431 = FALSE,
    status_code_451 = FALSE,
    ## 500
    status_code_500 = FALSE,
    status_code_501 = FALSE,
    status_code_502 = FALSE,
    status_code_503 = FALSE,
    status_code_504 = FALSE,
    status_code_505 = FALSE,
    status_code_506 = FALSE,
    status_code_507 = FALSE,
    status_code_508 = FALSE,
    status_code_510 = FALSE,
    status_code_511 = FALSE
  )
}

##' Checker Options
##'
##' @details Customize which HTTP status codes and other situations lead checker
##'   to mark links and other URIs as invalid/broken.
##'
##' Options set to TRUE will be interpreted by checker as valid links. For
##' instance, you will most likely always want `status_code_404 = FALSE`, so
##' checker will report as invalid 404 HTTP status codes.
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
  res <- utils::modifyList(def, usr)

  if (!all(purrr::map_lgl(res, is.logical)) ||
        any(is.na(res)))
    stop("all checker_options values must be TRUE or FALSE")

  res
}
