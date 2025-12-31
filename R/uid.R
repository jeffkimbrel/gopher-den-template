make_object_id <- function(object_type, short_id) {
  paste0(object_type, ":", short_id)
}

make_object_uid <- function(object_id) {
  digest::digest(object_id,
                 algo = "sha256",
                 serialize = FALSE)
}
