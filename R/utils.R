convert_data_uri <- function(data_uri) {
  paste0(substr(data_uri, 1, 100), "...")
}

##' @importFrom dplyr case_when
get_uri_type <- function(scheme, server, ...) {
  dplyr::case_when(
    scheme == "data" ~ "data",
    scheme == "mailto" ~ "mailto",
    scheme == "" & server == "" ~ "local",
    scheme == "http" & server %in% c("localhost", "127.0.0.1", "0.0.0.0") ~ "localhost",
    TRUE ~ "external"
  )
}

##' @importFrom fs is_dir
assert_dir <- function(dir) {
  if (!identical(length(dir), 1L))
    stop("Make sure you only provide 1 path", call. = FALSE)
  if (!fs::is_dir(dir))
    stop(sQuote(dir), " doesn't seem to be a directory", call. = FALSE)
}


### semantically meaninful functions to deal with error codes
is_error <- function() return("error")
is_warning <- function() return("warning")
is_message <- function() return("message")
is_silent <- function() return("ok")
is_success <- function() return("success")
