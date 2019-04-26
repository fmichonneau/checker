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
                               ...
) {

  timeout <- as.difftime(timeout, units = "secs")
  deadline <- Sys.time() + timeout

  bundle_install <- withr::with_dir(site_root, {
    processx::run("bundle", "install")
  })
  if (verbose) message(bundle_install$stdout)

  jkyl <- withr::with_dir(site_root, {
    processx::process$new(
      "bundle",
      c("exec", "jekyll", "serve", "--port", jekyll_port),
      stdout = "|", stderr = "|")
  })

  while (jkyl$is_alive() && (now <- Sys.time()) < deadline) {
    poll_time <- as.double(deadline - now, units = "secs") * 1000
    jkyl$poll_io(as.integer(poll_time))
    lines <- jkyl$read_output_lines()
    if (verbose && !identical(length(nchar(lines)), 0L))
      message(cat(lines, sep = "\n"))
    if (any(grepl("server running", lines, ignore.case = TRUE))) {
      if (verbose) message("Jekyll is running just fine.")
      break
    }
  }

  on.exit(jkyl$kill(), add = TRUE)

  res_jekyll <- check_links(
    file.path(site_root, "_site"),
    root_dir = paste0("http://localhost:", jekyll_port),
    ...
  )
  invisible(res_jekyll)
}
