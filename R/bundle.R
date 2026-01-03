write_bundle <- function(path,
                         tables = NULL,
                         sheet_order = c("study",
                                         "site",
                                         "sample",
                                         "readset",
                                         "assembly",
                                         "mag",
                                         "measurement",
                                         "qc",
                                         "qc_run",
                                         "taxonomy",
                                         "test",
                                         "taxonomy_run",
                                         "edge"),
                         protected = NULL,
                         overwrite = TRUE,
                         excluded = c("object"),
                         db_path = NULL,
                         db_file = getOption("gopheR.db_file", "gopheR_db.sqlite")) {

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
    gopheR::gopher_query(
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

    gopheR::gopher_query(
      con,
      sprintf("SELECT name FROM pragma_table_info('%s') ORDER BY cid;", tbl)
    ) |>
      dplyr::pull(name)
  }

  # ---- 3) Query schema + build sheets inside with_gopher_con()
  sheets <- gopheR::with_gopher_con(
    path = db_path,
    db = db_file,
    read_only = TRUE,
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

  order_sheet_list <- function(sheet_list, sheet_order) {
    stopifnot(is.list(sheet_list), !is.null(names(sheet_list)))
    stopifnot(is.character(sheet_order))

    nm <- names(sheet_list)

    # names in preferred order that actually exist
    in_order <- sheet_order[sheet_order %in% nm]

    # any remaining sheets not mentioned in sheet_order
    leftovers <- setdiff(nm, sheet_order)

    sheet_list[c(in_order, leftovers)]
  }

  # order them logically
  sheets <- order_sheet_list(sheets, sheet_order)



  openxlsx::write.xlsx(sheets, out_path, overwrite = overwrite)
  invisible(out_path)
}







#' Read, validate, and ingest a filled Excel bundle
#'
#' @param xlsx Path to the filled bundle .xlsx
#' @param quiet If TRUE, suppress messages (where applicable)
#'
#' @return A list containing bundle + validation/ingestion summaries

ingest_bundle <- function(
    xlsx,
    insert = FALSE,
    quiet = FALSE
) {

  if (!quiet) message("Reading bundle: ", xlsx)
  bundle <- read_bundle(xlsx)

  if (!quiet) message("Validating object IDs...")
  validate_bundle_object_ids(bundle)

  if (!quiet) message("Validating edge references...")
  validate_bundle_edge_refs(bundle, ref_cols = ref_cols)

  if (!quiet) message("Finalizing objects...")
  object_rows <- bundle_to_object_rows(bundle, id_cols)


  # build edges
  if (!quiet) message("Building edges...")
  edge_rows <- bundle_to_edge_rows(bundle)

  if (!quiet) message("Validating edge endpoints exist in objects...")
  validate_edge_endpoints_exist(edge_rows, object_rows)



  if (insert == TRUE) {
    # after all validation is done
    if (!quiet) message("Inserting objects...")
    obj_res <- insert_objects(object_rows)
    edge_res <- insert_edges(edge_rows)
    types_res = insert_types(bundle)
    results_res = insert_results(
      bundle,
      run_tables = c("mag_qc_run", "mag_taxonomy_run"),
      tables     = c("mag_qc", "mag_taxonomy", "measurement")
    )


    if (!quiet) {
      # obj_res is invisible(list(summary=..., object_rows=...)) from our implementation
      # printing a nice one-liner is handy
      try(
        {
          msg <- paste0(obj_res$summary$object_type, "=", obj_res$summary$n, collapse = ", ")
          message("Objects ingested: ", msg)
        },
        silent = TRUE
      )
    }

    invisible(list(
      bundle = bundle,
      objects = obj_res
    ))
  } else {
    message("Test run only...")
  }
}






#' Read a bundled .xlsx file
#'
#' @param xlsx path to the bundle.xlsx file
#' @param tables list of table to include (default: NULL for all tables)
#' @param db_path db path
#' @param db_file db file
#'
#' @returns a list
#' @export

read_bundle <- function(xlsx,
                        tables = NULL,
                        db_path = getOption("gopher.db_path"),
                        db_file = getOption("gopher.db_file")) {

  xlsx_path <- path.expand(xlsx)

  if (is.null(tables)) {
    tables <- openxlsx::getSheetNames(xlsx_path)
  }

  # verify the sheet names are truly found in the database
  gopheR::with_gopher_con(path = db_path,
                          db = db_file,
                          read_only = TRUE,
                          .f = \(con) {
                            db_tables <- gopher_table_names(con)
                            bad <- setdiff(tables, db_tables)

                            if (length(bad) > 0) {
                              stop("Unknown sheet(s) (not tables in DB): ",
                                   paste(bad, collapse = ", "), call. = FALSE)
                            }
                          }
  )


  data = tables |>
    purrr::set_names() |>
    purrr::map(\(tbl) {
      openxlsx::read.xlsx(xlsx_path, sheet = tbl) |>
        tibble::as_tibble() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), \(x) {
          if (is.character(x)) stringr::str_squish(dplyr::na_if(x, "")) else x
        }))
    })

}
