validate_bundle_object_ids <- function(bundle,
                                       db_path = NULL,
                                       db_file = getOption("gopheR.db_file", "gopheR_db.sqlite"),
                                       object_table = "object",
                                       object_id_col = "object_id") {

  # only validate object-like tables
  bundle_obj <- bundle[names(bundle) %in% object_tables]

  # internal function to get the name of the ID columns in each object table
  find_id_col <- function(df) {
    id_cols <- names(df)[stringr::str_detect(names(df), "_id$")]
    if (length(id_cols) != 1) return(NA_character_)
    id_cols[[1]]
  }

  # and here is where we are using it to get the ids.
  ids <- bundle_obj |>
    purrr::imap_dfr(\(df, tbl) {
      id_col <- find_id_col(df)

      if (is.na(id_col)) {
        return(tibble::tibble(
          table = tbl, type = "missing_or_ambiguous_id_col",
          id_col = NA_character_, raw_id = NA_character_, object_id = NA_character_
        ))
      }

      df |>
        dplyr::transmute(
          table = tbl,
          type = "candidate",
          id_col = id_col,
          raw_id = as.character(.data[[id_col]]) |>
            stringr::str_squish() |>
            dplyr::na_if(""),
          object_id = paste0(tbl, ":", raw_id)
        ) |>
        dplyr::filter(!is.na(raw_id))
    })

  # and give an error if there are "structural issues"
  ## for example, if an object table does not have a *_id column, or if it has two or more
  structural_issues <- ids |>
    dplyr::filter(type != "candidate") |>
    dplyr::distinct()
  if (nrow(structural_issues) > 0) return(structural_issues)

  # check if there are duplicates within a table
  dup_within <- ids |>
    dplyr::count(table, raw_id, name = "n") |>
    dplyr::filter(n > 1) |>
    dplyr::mutate(type = "duplicate_within_sheet") |>
    dplyr::select(table, type, raw_id, n)

  # check if there are duplicate raw_ids across the table
  dup_across <- ids |>
    unique() |>
    dplyr::count(raw_id, name = "n") |>
    dplyr::filter(n > 1) |>
    dplyr::mutate(type = "duplicate_across_bundle") |>
    dplyr::select(type, raw_id, n)

  # check if the *_id is already in the database
  existing <- gopheR::with_gopher_con(path = db_path, db = db_file, read_only = TRUE, .f = \(con) {
    gopheR::gopher_query(
      con,
      sprintf("SELECT %s AS object_id FROM %s", object_id_col, object_table)
    ) |>
      dplyr::pull(object_id) |>
      unique()
  })

  already_in_db <- ids |>
    dplyr::filter(object_id %in% existing) |>
    dplyr::distinct(object_id) |>
    dplyr::mutate(type = "already_in_db") |>
    dplyr::select(type, object_id)

  result = dplyr::bind_rows(dup_within, dup_across, already_in_db)

  if (nrow(result) > 0) {
    print(result)
    stop("validation error", call. = F)
  } else {
    return(result)
  }
}




validate_bundle_edge_refs <- function(bundle,
                                      ref_cols,
                                      db_path = NULL,
                                      db_file = getOption("gopheR.db_file", "gopheR_db.sqlite"),
                                      object_table = "object",
                                      object_id_col = "object_id") {

  # Atomic character helper (handles list-columns coming from Excel)
  as_chr_atomic <- function(x) {
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    as.character(x)
  }

  # Find exactly one *_id column in a sheet
  find_id_col <- function(df) {
    id_cols <- names(df)[stringr::str_detect(names(df), "_id$")]
    if (length(id_cols) != 1) return(NA_character_)
    id_cols[[1]]
  }

  # Robustly parse "comma lists" from Excel cells (also tolerates c("a","b"))
  extract_raw_ids <- function(x) {
    as_chr_atomic(x) |>
      stringr::str_squish() |>
      stringr::str_replace("^c\\((.*)\\)$", "\\1") |>
      stringr::str_replace_all("[\"']", "") |>
      stringr::str_split(",") |>
      (\(lst) unlist(lst, use.names = FALSE))() |>
      stringr::str_squish() |>
      (\(v) v[!is.na(v) & v != ""])() |>
      unique()
  }


  # (A) object_ids that already exist in the DB
  existing <- gopheR::with_gopher_con(\(con) {
    gopheR::gopher_query(
      con,
      sprintf("SELECT %s AS object_id FROM %s", object_id_col, object_table)
    ) |>
      dplyr::pull(object_id) |>
      unique()
  })

  # (B) object_ids introduced by this bundle (object tables only)
  bundle_ids <- bundle[names(bundle) %in% object_tables] |>
    purrr::imap_dfr(\(df, tbl) {
      id_col <- find_id_col(df)
      if (is.na(id_col)) return(tibble::tibble(object_id = character(0)))

      raw <- df[[id_col]] |>
        as_chr_atomic() |>
        stringr::str_squish() |>
        dplyr::na_if("")

      tibble::tibble(object_id = paste0(tbl, ":", raw)) |>
        dplyr::filter(!is.na(object_id))
    }) |>
    dplyr::pull(object_id) |>
    unique()

  valid <- union(existing, bundle_ids)

  # (C) validate refs for each configured column
  result <- purrr::imap_dfr(ref_cols, \(col_map, sheet) {
    df <- bundle[[sheet]]
    if (is.null(df)) return(tibble::tibble())

    purrr::imap_dfr(col_map, \(target_tbl, col) {
      if (!(col %in% names(df))) return(tibble::tibble())

      refs <- extract_raw_ids(df[[col]])
      if (length(refs) == 0) return(tibble::tibble())

      ref_object_ids <- paste0(target_tbl, ":", refs)
      missing <- setdiff(ref_object_ids, valid)

      if (length(missing) == 0) return(tibble::tibble())

      tibble::tibble(
        table = sheet,
        field = col,
        target_table = target_tbl,
        type = "missing_reference",
        missing_object_id = missing
      )
    })
  })

  if (nrow(result) > 0) {
    print(result)
    stop("validation error", call. = F)
  } else {
    return(result)
  }
}



