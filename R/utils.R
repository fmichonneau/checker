make_path_rel <- function(.data, dir, show_full_path) {
  if (show_full_path)
    return(.data)

  dir <- normalizePath(dir)

  .data %>%
    dplyr::mutate(
      file = gsub(dir, ".", .data$file)
    )
}
