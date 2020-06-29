##' @title Check links on Travis
##'
##' Supports only Jekyll-based sites for now
##'
##' @param site_root path for the site
##' @param jekyll_port which port to use
##' @param ruby_cmd ruby environment command that preceeds the call (e.g., `rvm
##'   2.5.8 do`)
##' @param timeout how long to wait for jekyll to sucessfully build the site (in
##'   seconds)
##' @param verbose should information about progress of the jekyll build be
##'   displayed?
##' @param ... additional arguments to be passed to check_links
##' @export
check_jekyll_links <- function(site_root = ".",
                               jekyll_port = "4000",
                               ruby_cmd = NULL,
                               timeout = 1000,
                               verbose = TRUE,
                               use_bundle = TRUE,
                               ...
) {

  site_root <- normalizePath(site_root, mustWork = TRUE)

  timeout <- as.difftime(timeout, units = "secs")
  deadline <- Sys.time() + timeout

  has_gemfile <- fs::file_exists(file.path(site_root, "Gemfile"))

  if (!has_gemfile && use_bundle) {
    stop("No Gemfile found in ", sQuote(site_root), ". ", call. = FALSE)
  }

  if (use_bundle) {
    default_cmd <- "bundle"

    ## bundle config
    config_args <- c(
      "config", "set", "path",
      file.path(tempdir(), ".vendor", "bundle")
    )
    ruby_config_env <- parse_ruby_cmd(ruby_cmd, default_cmd, config_args)
    bundle_config <- withr::with_dir(site_root, {
      processx::run(ruby_config_env$cmd, ruby_config_env$args)
    })
    if (verbose) message(bundle_config$stdout)

    ## bundle install
    install_args <- c("install")
    ruby_install_env <- parse_ruby_cmd(ruby_cmd, default_cmd, install_args)
    bundle_install <- withr::with_dir(site_root, {
      processx::run(ruby_install_env$cmd, ruby_install_env$args)
    })
    if (verbose) message(bundle_install$stdout)

    ## bundle update
    update_args <- c("update")
    ruby_update_env <- parse_ruby_cmd(ruby_cmd, default_cmd, update_args)
    bundle_update <- withr::with_dir(site_root, {
      processx::run(ruby_update_env$cmd, ruby_update_env$args)
    })
    if (verbose) message(bundle_update$stdout)

    ## bundle exec jekyll serve
    serve_args <- c("exec", "jekyll", "serve", "--port", jekyll_port)
    ruby_serve_env <- parse_ruby_cmd(ruby_cmd, default_cmd, serve_args)

    jkyl <- withr::with_dir(site_root, {
      processx::process$new(
        ruby_serve_env$cmd,
        ruby_serve_env$args,
        stdout = "|", stderr = "|")
    })
  } else {
    ## TODO: implement ruby command logic here as well.
    jkyl <- withr::with_dir(site_root, {
      processx::process$new(
        "jekyll",
        c("serve", "--port", jekyll_port),
        stdout = "|", stderr = "|"
      )
    })
  }

  while (jkyl$is_alive() && (now <- Sys.time()) < deadline) {
    poll_time <- as.double(deadline - now, units = "secs") * 1000
    jkyl$poll_io(as.integer(poll_time))
    lines <- jkyl$read_output_lines()
    if (verbose && !identical(length(nchar(lines)), 0L))
      message(cat(lines, sep = "\n"))
    if (any(grepl("server running", lines, ignore.case = TRUE))) {
      if (verbose) message("Jekyll is running.")
      break
    }
    if (any(grepl("error", lines, ignore.case = TRUE))) {
      stop("Jekyll error: ", lines, call. = FALSE)
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


parse_ruby_cmd <- function(ruby_cmd = NULL, cmd, args) {
  if (!is.null(ruby_cmd)) {
    ruby_cmd <- unlist(strsplit(ruby_cmd, " "))
    ry_cmd <- ruby_cmd[1]
    ry_args <- ruby_cmd[-1]

    cmd <- ry_cmd
    args <- c(ry_args, cmd, args)
  }
  message(
    "Command: ", paste(cmd, collapse = " "), "\n",
    "Args: ", paste(args, collapse = " ")
  )
  list(
    cmd = cmd,
    args = args
  )
}
