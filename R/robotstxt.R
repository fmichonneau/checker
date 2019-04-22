##' @importFrom dplyr filter distinct mutate group_nest select left_join
##' @importFrom robotstxt robotstxt
get_robotstxt <- function(input) {

  rbt <- input %>%
    dplyr::filter(.data$uri_type == "external") %>%
    dplyr::distinct(.data$scheme, .data$server, .data$path) %>%
    dplyr::mutate(full_server = paste0(.data$scheme, "://", .data$server)) %>%
    dplyr::group_nest(.data$full_server)

  rbt <- rbt %>%
    dplyr::mutate(is_allowed = purrr::pmap(rbt,
      function(full_server, data, ...) {
        rbt <- try(suppressMessages(
          robotstxt::robotstxt(full_server, warn = FALSE)
        ), silent = TRUE)

        if (inherits(rbt, "try-error"))
          return(rep(TRUE, length(data$path)))

        rbt$check(paths = data$path)
      })) %>%
    tidyr::unnest() %>%
    dplyr::select(-.data$full_server)

  dplyr::left_join(input, rbt, by = c("scheme", "server", "path"))
}
