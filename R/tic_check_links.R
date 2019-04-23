##' @title Check links on Travis
##'
##' Supports only Jekyll-based sites for now
##'
##' @param site_root path for the site
##' @param jekyll_port which port to use
##' @param timeout how long to wait for jekyll to sucessfully build the site (in
##'   seconds)
##' @param verbose should information about progress of the jekyll build be
##'   displayed?
##' @param ... additional arguments to be passed to check_links
##' @export
check_jekyll_links <- function(site_root = ".",
                               jekyll_port = "4000",
                               timeout = 1000,
                               verbose = TRUE,
                               recursive = TRUE,
                               only_with_issues = FALSE,
                               show_summary = TRUE
                               ) {
  CheckJekyllLinks$new(site_root, jekyll_port, timeout, verbose,
    recursive, only_with_issues, show_summary)
}


CheckJekyllLinks <- R6::R6Class(
  "CheckJekyllLinks", inherit = tic::TicStep,
  public = list(
    initialize = function(site_root, jekyll_port, timeout, verbose,
                          recursive, only_with_issues, show_summary) {
      private$site_root <- site_root
      private$jekyll_port <- jekyll_port
      private$timeout <- timeout
      private$verbose <- verbose
      private$recursive <- recursive
      private$only_with_issues <- only_with_issues
      private$show_summary <- show_summary

      super$înitialize()
    },
    run = function() {

      timeout <- as.difftime(private$timeout, units = "secs")
      deadline <- Sys.time() + timeout

      bundle_install <- withr::with_dir(private$site_root, {
        processx::run("bundle", "install")
      })
      if (private$verbose) message(bundle_install$stdout)

      jkyl <- withr::with_dir(private$site_root, {
        processx::process$new(
          "bundle",
          c("exec", "jekyll", "serve", "--port", private$jekyll_port),
          stdout = "|", stderr = "|")
      })

      while (jkyl$is_alive() && (now <- Sys.time()) < deadline) {
        poll_time <- as.double(deadline - now, units = "secs") * 1000
        jkyl$poll_io(as.integer(poll_time))
        lines <- jkyl$read_output_lines()
        if (private$verbose) message(cat(lines, sep = "\n"))
        if (any(grepl("server running", lines, ignore.case = TRUE))) {
          if (private$verbose) message("Jekyll is running just fine.")
          break
        }
      }

      on.exit(jkyl$kill(), add = TRUE)

      res_jekyll <- check_links(
        file.path(private$site_root, "_site"),
        root_dir = paste0("http://localhost:", private$jekyll_port),
        recursive = private$recursive,
        only_with_issues = private$only_with_issues,
        show_summary = private$show_summary
      )

    }),

  private = list(
    site_root = NULL,
    jekyll_port = NULL,
    timeout = NULL,
    verbose = NULL,
    recursive = NULL,
    only_with_issues = NULL,
    show_summary = NULL
  )
)
