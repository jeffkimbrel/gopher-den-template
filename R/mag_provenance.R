#' MAG provenance (connection-scoped)
#'
#' Internal helper that runs the query using an existing DBI connection.
#' Most callers should use [mag_provenance()] instead, which safely manages
#' the connection lifecycle.
#'
#' @param con A DBI connection (opened elsewhere).
#' @param mag_id Character scalar. MAG identifier (without the `mag:` prefix).
#'
#' @returns A data frame describing MAG provenance (assembly/readset/sample context).
#'
#' @keywords internal
mag_provenance_con <- function(con, mag_id) {
  stopifnot(is.character(mag_id), length(mag_id) == 1L, nzchar(mag_id))

  # Use parameter binding for safety + correct quoting.
  # We also bind the "mag:{mag_id}" UID rather than interpolating it.
  gopheR::gopher_query(con,
    "
    SELECT
      ? AS mag_id,
      substr(e1.parent_uid, instr(e1.parent_uid, ':') + 1) AS assembly_id,
      substr(e2.parent_uid, instr(e2.parent_uid, ':') + 1) AS readset_id,
      e2.role,
      r.sample_id,
      s.site,
      s.collected_date,
      s.biome
    FROM edge e1
    JOIN edge e2
      ON e2.child_uid = e1.parent_uid
     AND e2.edge_type = 'assembled_from'
    JOIN read_set r
      ON r.readset_id = substr(e2.parent_uid, instr(e2.parent_uid, ':') + 1)
    JOIN sample s
      ON s.sample_id = r.sample_id
    WHERE e1.edge_type = 'binned_from'
      AND e1.child_uid = ?
    ORDER BY assembly_id, readset_id
    ",
    params = list(mag_id, paste0("mag:", mag_id))
  )
}

#' MAG provenance
#'
#' Returns provenance information for a MAG, linking it to the assembly it was binned
#' from and the read sets (and samples) that contributed to that assembly.
#'
#' This function opens and closes the database connection automatically. For advanced
#' use (e.g., batching multiple queries on a single connection), see
#' [mag_provenance_con()].
#'
#' @param mag_id Character scalar. MAG identifier (without the `mag:` prefix).
#' @param path Character scalar. Directory containing the database file.
#'   If `NULL`, uses `getOption("gopheR.db_path")` or `Sys.getenv("GOPHER_DB_PATH")`.
#' @param db Character scalar. Database filename (default `"gopheR_db.sqlite"`).
#' @param read_only Logical. If `TRUE` (default), attempts to open the SQLite database
#'   in read-only mode.
#'
#' @returns A data frame describing MAG provenance (assembly/readset/sample context).
#'
#' @export
mag_provenance <- function(mag_id, path = NULL, db = "gopheR_db.sqlite", read_only = TRUE) {
  gopheR::with_gopher_con(
    function(con) mag_provenance_con(con, mag_id = mag_id),
    path = path,
    db = db,
    read_only = read_only
  )
}
