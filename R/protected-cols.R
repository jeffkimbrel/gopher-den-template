#' Default protected (non-editable) columns per table
#'
#' @keywords internal

gopher_protected_cols_default <- function() {
  list(
    sample   = c("sample_uid", "created_at", "updated_at"),
    readset = c("readset_uid", "created_at", "updated_at"),
    assembly = c("assembly_uid", "created_at", "updated_at"),
    measurement = c("measurement_uid", "created_at", "updated_at"),
    edge     = c("edge_uid", "created_at", "updated_at"),
    mag      = c("mag_uid", "created_at", "updated_at")
  )
}

#' @keywords internal

protected_cols_for <- function(table, protected = NULL) {
  defaults <- gopher_protected_cols_default()

  user_tbl <- protected[[table]]
  default_tbl <- defaults[[table]]

  if (is.null(default_tbl)) default_tbl <- character(0)
  if (is.null(user_tbl)) user_tbl <- character(0)

  # user entries ADD to defaults (can’t “unprotect” in v1)
  union(default_tbl, user_tbl)
}
