#' MAG occurrences across samples (connection-scoped)
#'
#' Internal helper that runs the query using an existing DBI connection.
#' Most callers should use [mag_sample_occurrences()] instead, which safely
#' manages the connection lifecycle.
#'
#' @param con A DBI connection (opened elsewhere).
#'
#' @returns A data frame with MAG/sample occurrence rows.
#'
#' @keywords internal

mag_sample_occurrences_con <- function(con) {
  gopheR::gopher_query(
    con,
    "
    SELECT
      substr(e.child_uid, instr(e.child_uid, ':') + 1)  AS mag_id,
      substr(e.parent_uid, instr(e.parent_uid, ':') + 1) AS readset_id,
      r.sample_id,
      s.site,
      s.collected_date,
      s.biome,
      meas.value AS moisture,
      meas.unit  AS moisture_unit,
      e.weight   AS rel_abundance
    FROM edge e
    JOIN read_set r
      ON r.readset_id = substr(e.parent_uid, instr(e.parent_uid, ':') + 1)
    JOIN sample s
      ON s.sample_id = r.sample_id
    LEFT JOIN measurement meas
      ON meas.sample_id = s.sample_id
     AND meas.variable = 'moisture'
    WHERE e.edge_type = 'observed_in'
    ORDER BY mag_id, rel_abundance DESC
    "
  )
}

#' MAG occurrences across samples
#'
#' Returns the occurrence of MAGs across samples/read sets, including sample
#' metadata and (when available) moisture measurements.
#'
#' This function opens and closes the database connection automatically.
#' For advanced use (e.g., batching multiple queries on a single connection),
#' see the internal helper `mag_sample_occurrences_con()`.
#'
#' @param path Character scalar. Directory containing the database file.
#'   If `NULL`, uses `getOption("gopheR.db_path")` or `Sys.getenv("GOPHER_DB_PATH")`.
#' @param db Character scalar. Database filename (default `"gopheR_db.sqlite"`).
#' @param read_only Logical. If `TRUE` (default), attempts to open the SQLite
#'   database in read-only mode.
#'
#' @returns A data frame with MAG/sample occurrence rows.
#'
#' @export

mag_sample_occurrences <- function(path = NULL, db = "gopheR_db.sqlite", read_only = TRUE) {
  gopheR::with_gopher_con(
    function(con) mag_sample_occurrences_con(con),
    path = path,
    db = db,
    read_only = read_only
  )
}
