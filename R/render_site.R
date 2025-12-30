#' Render the Quarto site bundled with this package
#'
#' This renders the Quarto project located at `inst/quarto/`.
#' It works both when the package is installed (uses `system.file()`)
#' and during development (uses the repo's `inst/quarto/`).
#'
#' @param output_dir Where to write the rendered site. If NULL, Quarto uses its default
#'   (usually `_site/` inside the site directory).
#' @param site_dir Override the site directory. If NULL, it auto-detects.
#' @param quiet If TRUE, suppress Quarto output.
#' @param ... Reserved for future options.
#'
#' @returns Invisibly returns the path to the rendered output directory (best effort).
#' Render the Quarto site bundled with this package
#' @export

render_site <- function(site_dir = "inst/quarto", output_dir = NULL) {

  if (Sys.which("quarto") == "") {
    stop("Quarto CLI not found on PATH.", call. = FALSE)
  }

  site_dir <- normalizePath(site_dir, mustWork = TRUE)

  args <- c("render", site_dir)
  if (!is.null(output_dir)) {
    args <- c(args, "--output-dir", normalizePath(output_dir, mustWork = FALSE))
  }

  args <- c(args, "--no-cache")

  message("Rendering Quarto site at: ", site_dir)
  status <- system2("quarto", args = args)

  if (status != 0) {
    stop("Quarto render failed.", call. = FALSE)
  }

  invisible(if (is.null(output_dir)) file.path(site_dir, "_site") else output_dir)
}
