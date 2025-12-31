write_bundle <- function(path,
                         tables = NULL,
                         protected = NULL,
                         overwrite = TRUE,
                         excluded = c("object"),
                         db_path = NULL,
                         db_file = getOption("gopheR.db_file", "gopheR_db.sqlite"),
                         read_only = TRUE) {

  stopifnot(is.character(path), length(path) == 1)
  stopifnot(is.logical(overwrite), length(overwrite) == 1)

  if (!is.null(tables)) {
    stopifnot(is.character(tables), length(tables) >= 1)
  }
  if (is.null(protected)) protected <- list()

  # ---- 1) Resolve output path (directory -> bundle.xlsx; no ext -> add .xlsx)
  path_expanded <- path.expand(path)
  is_dir <- dir.exists(path_expanded)
  has_xlsx_ext <- grepl("\\.xlsx$", path_expanded, ignore.case = TRUE)

  out_path <- dplyr::case_when(
    is_dir ~ file.path(path_expanded, "bundle.xlsx"),
    !has_xlsx_ext ~ paste0(path_expanded, ".xlsx"),
    TRUE ~ path_expanded
  )

  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ---- 2) Local helpers (built-in functions)
  assert_safe_table <- function(tbl) {
    ok <- grepl("^[A-Za-z][A-Za-z0-9_]*$", tbl)
    if (!ok) stop("Unsafe table name: ", tbl, call. = FALSE)
    invisible(tbl)
  }

  empty_template <- function(cols) {
    stats::setNames(
      replicate(length(cols), logical(0), simplify = FALSE),
      cols
    ) |>
      tibble::as_tibble()
  }

  # SQLite table discovery (via gopher_query)
  gopher_table_names <- function(con) {
    gopher_query(
      con,
      "
      SELECT name
      FROM sqlite_master
      WHERE type = 'table'
        AND name NOT LIKE 'sqlite_%'
      ORDER BY name
      "
    ) |>
      dplyr::pull(name)
  }

  # SQLite column discovery (PRAGMA can't bind identifier; validate + interpolate)
  gopher_table_cols <- function(con, tbl) {
    assert_safe_table(tbl)

    gopher_query(
      con,
      sprintf("SELECT name FROM pragma_table_info('%s') ORDER BY cid;", tbl)
    ) |>
      dplyr::pull(name)
  }

  # ---- 3) Query schema + build sheets inside with_gopher_con()
  sheets <- gopheR::with_gopher_con(
    path = db_path,
    db = db_file,
    read_only = read_only,
    .f = \(con) {
      tbls <- tables
      if (is.null(tbls)) tbls <- gopher_table_names(con)

      tbls = setdiff(tbls, excluded)

      tbls |>
        purrr::set_names() |>
        purrr::map(\(tbl) {
          cols <- gopher_table_cols(con, tbl)

          # protected_cols_for(table, protected = protected)
          protected_tbl <- protected_cols_for(tbl, protected = protected)
          editable <- setdiff(cols, protected_tbl)

          # add additional columns
          editable <- c(editable, additional_cols_for(tbl))


          if (length(editable) == 0) {
            tibble::tibble(.note = "No editable columns for this table.")
          } else {
            empty_template(editable)
          }
        })
    }
  )

  openxlsx::write.xlsx(sheets, out_path, overwrite = overwrite)
  invisible(out_path)
}







#' @keywords internal

protected_cols_for <- function(table, protected = NULL) {
  defaults <- gopher_protected_cols_default

  user_tbl <- protected[[table]]
  default_tbl <- defaults[[table]]

  if (is.null(default_tbl)) default_tbl <- character(0)
  if (is.null(user_tbl)) user_tbl <- character(0)

  # user entries ADD to defaults (can’t “unprotect” in v1)
  union(default_tbl, user_tbl)
}



#' @keywords internal

additional_cols_for <- function(table, protected = NULL) {
  defaults <- gopher_additional_cols_default

  user_tbl <- protected[[table]]
  default_tbl <- defaults[[table]]

  if (is.null(default_tbl)) default_tbl <- character(0)
  if (is.null(user_tbl)) user_tbl <- character(0)

  # user entries ADD to defaults (can’t “unprotect” in v1)
  union(default_tbl, user_tbl)
}
