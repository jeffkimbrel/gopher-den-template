#' Read, validate, and ingest a filled Excel bundle
#'
#' @param xlsx Path to the filled bundle .xlsx
#' @param quiet If TRUE, suppress messages (where applicable)
#'
#' @return A list containing bundle + validation/ingestion summaries

ingest_bundle <- function(
  xlsx,
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










  # after all validation is done
  if (!quiet) message("Inserting objects...")
  obj_res <- insert_objects(object_rows)



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
}
