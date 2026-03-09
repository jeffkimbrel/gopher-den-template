#' Generate per-MAG Quarto pages from a template
#'
#' @param out_dir Output directory for generated .qmd files.
#' @param site_root Root of the Quarto site in the repo. Defaults to package dev root if found,
#'   otherwise to the installed package site directory.
#' @param db_dir Directory containing the DB file (relative to site_root). Default "data".
#' @param db Name of the DB file. Default "gopheR_db.sqlite".
#' @param template_rel Relative path (from site_root) to the MAG template QMD.
#' @param ... Passed through to `with_gopher_con()` (if you support path/db/read_only there).
#'
#' @export

build_mag_pages <- function(out_dir = "inst/quarto/mag",
                            site_root = NULL,
                            db_dir = "inst/quarto/data",
                            db = "gopheR_db.sqlite",
                            template_rel = file.path("inst", "quarto", "mag", "_template_mag.qmd"),
                            ...) {

  # Find a sensible root without `here()`
  find_pkg_root <- function() {
    p <- normalizePath(".", winslash = "/", mustWork = TRUE)
    for (i in 1:20) {
      if (file.exists(file.path(p, "DESCRIPTION"))) return(p)
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
    NULL
  }

  if (is.null(site_root)) {
    # dev root if in repo
    site_root <- find_pkg_root()
    # fallback to installed package files (if present)
    if (is.null(site_root)) {
      site_root <- system.file("", package = utils::packageName())
      if (!nzchar(site_root)) {
        stop("Cannot determine site_root. Provide `site_root=`.", call. = FALSE)
      }
    }
  }

  template_file <- file.path(site_root, template_rel)
  if (!file.exists(template_file)) {
    stop("Template not found: ", template_file, call. = FALSE)
  }

  # Out dir relative to site_root unless absolute
  out_dir_full <- if (fs::is_absolute_path(out_dir)) out_dir else file.path(site_root, out_dir)
  fs::dir_create(out_dir_full)

  template <- readr::read_file(template_file)

  # Use engine connection helpers (path is a directory)
  db_path_dir <- file.path(site_root, db_dir)

  mags <- gopheR::with_gopher_con(function(con) {
    gopheR::gopher_query(con, "SELECT mag_id FROM mag ORDER BY mag_id")
  }, path = db_path_dir, db = db, read_only = TRUE, ...)

  for (id in mags$mag_id) {
    out <- stringr::str_replace_all(
      template,
      stringr::fixed("{{< meta mag_id >}}"),
      id
    )
    # safer YAML replace: only replace the specific default value line
    out <- stringr::str_replace(
      out,
      stringr::fixed('mag_id: "M001"'),
      paste0('mag_id: "', id, '"')
    )


    readr::write_file(out, file.path(out_dir_full, paste0(id, ".qmd")))
  }

  message("Generated ", nrow(mags), " MAG pages in ", out_dir_full)
  invisible(out_dir_full)
}
