generic_msg <- function(.dt = NULL,
                        msg, type = c("note", "warning", "error", "ok"),
                        indent = 0, ...) {

  type <- match.arg(type)

  icon <- switch(type,
                 "note" = cli::symbol$star,
                 "warning" = cli::symbol$warning,
                 "error" = cli::symbol$cross,
                 "ok" = cli::symbol$tick)

  col <- switch(type,
                "note" = crayon::blue,
                "warning" = crayon::yellow,
                "error" = crayon::red,
                "ok" = crayon::green)

  cat(col(
    crayon::bold(
      paste(rep(" ", indent), collapse = ""),
      icon, msg
    )))

  .dt

}


summary_checks <- function(.dt) {

  .dt


}
