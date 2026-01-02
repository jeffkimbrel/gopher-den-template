ref_cols <- list(
  assembly = c(assembled_from = "readset"),
  mag      = c(binned_from = "assembly", observed_in = "readset"),
  sample   = c(sampled_from = "site", derived_from = "sample", studied_in = "study"),
  readset  = c(sequenced_from = "sample"),
  site     = c(studied_in = "study")
)

id_cols <- c(
  study    = "study_id",
  site     = "site_id",
  sample   = "sample_id",
  readset  = "readset_id",
  assembly = "assembly_id",
  mag      = "mag_id"
)

object_tables = c("assembly","mag","readset","sample","site","study")

usethis::use_data(
  ref_cols,
  id_cols,
  object_tables,
  overwrite = TRUE
)

####
gopher_protected_cols_default <- list(
  assembly = c("created_at"),
  mag      = c("created_at"),
  readset =  c("created_at"),
  sample   = c("created_at"),
  site =     c("created_at"),
  study = c("created_at"),
  measurement = c("created_at", "measurement_uid"),
  edge     = c("created_at", "edge_uid"),
  mag_qc = c("created_at", "qc_uid"),
  mag_qc_run = c("created_at"),
  mag_taxonomy = c("created_at", "taxonomy_uid"),
  mag_taxonomy_run = c("created_at")
)


gopher_additional_cols_default <- list(
  assembly = c("assembled_from"),
  mag      = c("binned_from", "observed_in"),
  readset  = c("sequenced_from"),
  sample   = c("sampled_from", "studied_in", "derived_from"),
  site     = c("studied_in"),
  study    = character(0)
)

usethis::use_data(gopher_additional_cols_default,
                  gopher_protected_cols_default,
                  overwrite = TRUE)
