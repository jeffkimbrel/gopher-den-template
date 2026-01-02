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



#' List user-facing table (and optionally view) names in the gopher DB
#'
#' @param con A DBI connection.
#' @param include_views Logical; include views (TRUE) or tables only (FALSE).
#' @param include_internal Logical; include sqlite_* internal tables/views.
#' @param prefix Optional character scalar. If provided, only return names that
#'   start with this prefix (e.g. "gopher_" or "gopher."). Set NULL to disable.
#'
#' @return Character vector of table/view names.
#' @export
gopher_table_names <- function(con,
                               include_views = FALSE,
                               include_internal = FALSE,
                               prefix = NULL) {
  stopifnot(inherits(con, "DBIConnection"))

  types <- if (isTRUE(include_views)) c("table", "view") else "table"

  sql <- sprintf(
    "SELECT name
       FROM sqlite_master
      WHERE type IN (%s)
      ORDER BY name",
    paste(sprintf("'%s'", types), collapse = ", ")
  )

  out <- DBI::dbGetQuery(con, sql)$name
  out <- as.character(out)

  if (!isTRUE(include_internal)) {
    out <- out[!grepl("^sqlite_", out)]
  }

  if (!is.null(prefix)) {
    prefix <- as.character(prefix)[1]
    out <- out[startsWith(out, prefix)]
  }

  out
}

bundle_to_object_rows <- function(bundle, id_cols_by_type) {
  out <- list()

  for (typ in names(id_cols_by_type)) {
    if (!typ %in% names(bundle)) next

    df <- bundle[[typ]]
    id_col <- id_cols_by_type[[typ]]

    if (!id_col %in% names(df)) {
      stop(sprintf("For type '%s', id column '%s' not found in bundle[['%s']].",
                   typ, id_col, typ),
           call. = FALSE)
    }

    short_ids <- trimws(as.character(df[[id_col]]))
    short_ids <- short_ids[!is.na(short_ids) & nzchar(short_ids)]
    short_ids <- unique(short_ids)

    if (any(grepl("[:|]", short_ids))) {
      bad <- short_ids[grepl("[:|]", short_ids)]
      stop(sprintf(
        "Invalid %s IDs (cannot contain ':' or '|'): %s",
        typ, paste(bad, collapse = ", ")
      ), call. = FALSE)
    }

    out[[typ]] <- data.frame(
      object_type = typ,
      short_id    = short_ids,
      object_id   = paste0(typ, ":", short_ids),
      name        = short_ids,
      stringsAsFactors = FALSE
    )
  }

  if (length(out) == 0) {
    return(data.frame(
      object_type = character(0),
      short_id    = character(0),
      object_id   = character(0),
      name        = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, out) |>
    tibble::rownames_to_column("row_id") |>
    tibble::as_tibble()
}



insert_objects <- function(object_rows) {

  object_rows = object_rows |>
    tibble::column_to_rownames("row_id")

  if (nrow(object_rows) == 0) return(invisible(object_rows))

  gopheR::with_gopher_con(function(c) {

    if (!DBI::dbIsValid(c)) {
      stop("Invalid or closed gopher database connection.", call. = FALSE)
    }

    # Ensure schema exists (safe no-op if already there)
    DBI::dbExecute(c, "
    CREATE TABLE IF NOT EXISTS object (
      object_uid  TEXT PRIMARY KEY,
      object_id   TEXT NOT NULL UNIQUE,
      object_type TEXT NOT NULL,
      short_id    TEXT NOT NULL,
      name        TEXT NOT NULL,
      external_db TEXT,
      external_id TEXT,
      created_at  TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
    );
    ")

    DBI::dbExecute(c, "
    CREATE INDEX IF NOT EXISTS idx_object_type_short_id
      ON object(object_type, short_id);
    ")



    # create stable uids
    object_rows$object_uid <- vapply(
      object_rows$object_id,
      make_object_uid,
      FUN.VALUE = character(1)
    )

    # make sql statement
    sql <- "
      INSERT INTO object (
        object_uid,
        object_id,
        object_type,
        short_id,
        name,
        created_at
      )
      VALUES (
        :object_uid,
        :object_id,
        :object_type,
        :short_id,
        :name,
        strftime('%Y-%m-%dT%H:%M:%fZ','now')
      )
      ON CONFLICT(object_id) DO UPDATE SET
        object_uid  = excluded.object_uid,
        object_type = excluded.object_type,
        short_id    = excluded.short_id,
        name        = COALESCE(excluded.name, object.name);

      "

    DBI::dbExecute(c, "BEGIN")
    on.exit(try(DBI::dbExecute(c, "COMMIT"), silent = TRUE), add = TRUE)

    DBI::dbExecute(c, sql, params = object_rows)
  })


  # # Return a simple summary + the rows we attempted to ingest
  summary <- as.data.frame(table(object_rows$object_type), stringsAsFactors = FALSE)
  names(summary) <- c("object_type", "n")

  invisible(list(summary = summary, object_rows = object_rows))


  # invisible(object_rows)
}





insert_edges <- function(edge_rows, quiet = FALSE) {

  stopifnot(is.data.frame(edge_rows))

  req <- c("parent_id", "child_id", "edge_type")
  miss <- setdiff(req, names(edge_rows))

  if (length(miss)) {
    stop("edge_rows missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  # Normalize to character
  edge_rows <- edge_rows |>
    dplyr::mutate(
      parent_id = as.character(parent_id),
      child_id  = as.character(child_id),
      edge_type = as.character(edge_type)
    )

  # Deterministic UID per edge
  edge_rows <- edge_rows |>
    dplyr::rowwise() |>
    dplyr::mutate(edge_uid = make_object_uid(paste(parent_id, child_id, edge_type, sep = "|")))

  # Guard: duplicates inside the batch (same uid)
  dup_uids <- edge_rows$edge_uid[duplicated(edge_rows$edge_uid)]
  if (length(dup_uids) > 0) {
    ex <- edge_rows |> dplyr::filter(edge_uid %in% unique(dup_uids)) |> dplyr::slice_head(n = 10)
    stop(
      "Duplicate edges in incoming edge_rows (same edge_uid). ",
      "First few duplicates:\n",
      paste(capture.output(print(ex)), collapse = "\n"),
      call. = FALSE
    )
  }

  res <- gopheR::with_gopher_con(function(con) {
    # detect which columns exist in DB edge table, and only write those
    cols <- gopheR::gopher_query(con, "PRAGMA table_info(edge)")$name

    out <- edge_rows

    # Keep only columns that exist in the DB table
    out <- out[, intersect(names(out), cols), drop = FALSE]

    # Insert (let DB constraints handle duplicates against existing rows)
    DBI::dbWriteTable(con, "edge", out, append = TRUE)

    out
  })

  if (!quiet) message("Edges inserted: ", nrow(res))

  invisible(list(n = nrow(res), edge_rows = res))
}
