to_iso_utc <- function(x, tz = "UTC") {
  # vectorized
  out <- rep(NA_character_, length(x))

  is_blank <- is.na(x) | (is.character(x) & trimws(x) == "")
  if (all(is_blank)) return(out)

  # 1) Date / POSIXct
  is_date <- inherits(x, "Date")
  is_posix <- inherits(x, "POSIXct") || inherits(x, "POSIXt")

  if (is_date) {
    px <- as.POSIXct(x, tz = tz)
    out[!is_blank] <- format(px[!is_blank], "%Y-%m-%dT%H:%M:%OS3Z")
    return(out)
  }
  if (is_posix) {
    px <- as.POSIXct(x, tz = tz)
    out[!is_blank] <- format(px[!is_blank], "%Y-%m-%dT%H:%M:%OS3Z")
    return(out)
  }

  # Work in a character copy for pattern checks
  s <- as.character(x)
  s[is_blank] <- NA_character_

  # 2) Excel serial numbers: openxlsx can yield numeric for date cells
  # Excel day 1 = 1899-12-31 in Windows date system, but Excel has the 1900 leap-year bug.
  # A common, correct conversion in R is origin = "1899-12-30".
  is_numlike <- !is.na(s) & grepl("^[0-9]+(\\.[0-9]+)?$", s)
  if (any(is_numlike)) {
    num <- suppressWarnings(as.numeric(s[is_numlike]))
    # treat as Excel serial days (fractional part = time)
    px <- as.POSIXct(num * 86400, origin = "1899-12-30", tz = tz)
    out[is_numlike] <- format(px, "%Y-%m-%dT%H:%M:%OS3Z")
  }

  # remaining indices to parse as strings
  rem <- which(!is_blank & !is_numlike)

  if (length(rem) == 0) return(out)

  # 3) Already ISO with Z
  iso_z <- grepl("^\\d{4}-\\d{2}-\\d{2}T.*Z$", s[rem])
  out[rem[iso_z]] <- s[rem[iso_z]]

  rem2 <- rem[!iso_z]
  if (length(rem2) == 0) return(out)

  # 4) YYYY-MM-DD -> midnight
  ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}$", s[rem2])
  out[rem2[ymd]] <- paste0(s[rem2[ymd]], "T00:00:00.000Z")

  rem3 <- rem2[!ymd]
  if (length(rem3) == 0) return(out)

  # 5) YYYY-MM-DD HH:MM:SS(.fff) -> ISO + Z
  ymd_hms <- grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}(\\.\\d+)?$", s[rem3])
  out[rem3[ymd_hms]] <- paste0(sub(" ", "T", s[rem3[ymd_hms]], fixed = TRUE), "Z")

  # Anything else stays NA (or store as-is if you prefer)
  out
}






#' Generate per-MAG Quarto pages from a template
#'
#' @param out_dir Output directory for generated .qmd files.
#' @param site_root Root of the Quarto site in the repo. Defaults to package dev root if found,
#'   otherwise to the installed package site directory.
#' @param db_dir Directory containing the DB file (relative to site_root). Default "data".
#' @param db Name of the DB file. Default "gopheR_db.sqlite".
#' @param template_rel Relative path (from site_root) to the MAG template QMD.
#' @param ... Passed through to `with_gopher_con()` (if you support path/db/read_only there).
#'
#' @export

build_mag_pages <- function(out_dir = "inst/quarto/mag",
                            site_root = NULL,
                            db_dir = "inst/quarto/data",
                            db = "gopheR_db.sqlite",
                            template_rel = file.path("inst", "quarto", "mag", "_template_mag.qmd"),
                            ...) {

  # Find a sensible root without `here()`
  find_pkg_root <- function() {
    p <- normalizePath(".", winslash = "/", mustWork = TRUE)
    for (i in 1:20) {
      if (file.exists(file.path(p, "DESCRIPTION"))) return(p)
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
    NULL
  }

  if (is.null(site_root)) {
    # dev root if in repo
    site_root <- find_pkg_root()
    # fallback to installed package files (if present)
    if (is.null(site_root)) {
      site_root <- system.file("", package = utils::packageName())
      if (!nzchar(site_root)) {
        stop("Cannot determine site_root. Provide `site_root=`.", call. = FALSE)
      }
    }
  }

  template_file <- file.path(site_root, template_rel)
  if (!file.exists(template_file)) {
    stop("Template not found: ", template_file, call. = FALSE)
  }

  # Out dir relative to site_root unless absolute
  out_dir_full <- if (fs::is_absolute_path(out_dir)) out_dir else file.path(site_root, out_dir)
  fs::dir_create(out_dir_full)

  template <- readr::read_file(template_file)

  # Use engine connection helpers (path is a directory)
  db_path_dir <- file.path(site_root, db_dir)

  mags <- gopheR::with_gopher_con(function(con) {
    gopheR::gopher_query(con, "SELECT mag_id FROM mag ORDER BY mag_id")
  }, path = db_path_dir, db = db, read_only = TRUE, ...)

  for (id in mags$mag_id) {
    out <- stringr::str_replace_all(
      template,
      stringr::fixed("{{< meta mag_id >}}"),
      id
    )
    # safer YAML replace: only replace the specific default value line
    out <- stringr::str_replace(
      out,
      stringr::fixed('mag_id: "M001"'),
      paste0('mag_id: "', id, '"')
    )


    readr::write_file(out, file.path(out_dir_full, paste0(id, ".qmd")))
  }

  message("Generated ", nrow(mags), " MAG pages in ", out_dir_full)
  invisible(out_dir_full)
}





#' Render the Quarto site bundled with this package
#'
#' This renders the Quarto project located at `inst/quarto/`.
#' It works both when the package is installed (uses `system.file()`)
#' and during development (uses the repo's `inst/quarto/`).
#'
#' @param output_dir Where to write the rendered site. If NULL, Quarto uses its default
#'   (usually `_site/` inside the site directory).
#' @param site_dir Override the site directory. If NULL, it auto-detects.
#' @param quiet If TRUE, suppress Quarto output.
#' @param ... Reserved for future options.
#'
#' @returns Invisibly returns the path to the rendered output directory (best effort).
#' Render the Quarto site bundled with this package
#' @export

render_site <- function(site_dir = "inst/quarto", output_dir = NULL) {

  if (Sys.which("quarto") == "") {
    stop("Quarto CLI not found on PATH.", call. = FALSE)
  }

  site_dir <- normalizePath(site_dir, mustWork = TRUE)

  args <- c("render", site_dir)
  if (!is.null(output_dir)) {
    args <- c(args, "--output-dir", normalizePath(output_dir, mustWork = FALSE))
  }

  args <- c(args, "--no-cache")

  message("Rendering Quarto site at: ", site_dir)
  status <- system2("quarto", args = args)

  if (status != 0) {
    stop("Quarto render failed.", call. = FALSE)
  }

  invisible(if (is.null(output_dir)) file.path(site_dir, "_site") else output_dir)
}



make_object_id <- function(object_type, short_id) {
  paste0(object_type, ":", short_id)
}

make_object_uid <- function(object_id) {
  digest::digest(object_id,
                 algo = "sha256",
                 serialize = FALSE)
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
  existing <- gopheR::with_gopher_con(function(con) {
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






bundle_to_edge_rows <- function(bundle, split_pattern = "\\s*,\\s*", quiet = F) {

  stopifnot(is.list(bundle), is.character(id_cols), is.list(ref_cols))

  is_decorated_id <- function(x) grepl("^[^:]+:.+$", x)

  child_type_from_id <- function(x) sub(":.*$", "", x)
  parent_type_from_id <- function(x) sub(":.*$", "", x)

  parse_targets <- function(x) {
    if (is.null(x) || is.na(x)) return(character(0))
    x <- trimws(as.character(x))
    if (!nzchar(x)) return(character(0))
    parts <- unlist(strsplit(x, split_pattern, perl = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    parts[nzchar(parts)]
  }

  # 1) inferred edges (your existing logic)
  inferred <- purrr::imap_dfr(ref_cols, function(edge_map, source_tbl) {
    df <- bundle[[source_tbl]]
    if (is.null(df)) return(tibble::tibble())

    source_id_col <- unname(id_cols[[source_tbl]])
    if (is.null(source_id_col) || !source_id_col %in% names(df)) {
      stop(
        "Missing source id column for table '", source_tbl,
        "'. Expected: ", if (is.null(source_id_col)) "<NULL>" else source_id_col,
        call. = FALSE
      )
    }

    # edge columns that exist in this df
    edge_cols <- intersect(names(edge_map), names(df))
    if (length(edge_cols) == 0) return(tibble::tibble())

    # (optional) drop edge cols that are completely empty/blank
    edge_cols <- edge_cols[
      vapply(df[edge_cols], function(x) {
        x <- as.character(x)
        any(!is.na(x) & nzchar(trimws(x)))
      }, logical(1))
    ]
    if (length(edge_cols) == 0) return(tibble::tibble())

    long <- df |>
      dplyr::select(dplyr::all_of(c(source_id_col, edge_cols))) |>
      # critical: make pivoted cols consistent type (avoids char/double conflicts)
      dplyr::mutate(dplyr::across(dplyr::all_of(edge_cols), as.character)) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(edge_cols),
        names_to = "edge_type",
        values_to = "raw"
      ) |>
      dplyr::rename(source_id = dplyr::all_of(source_id_col)) |>
      dplyr::mutate(raw = as.character(raw))

    expanded <- long |>
      dplyr::mutate(target_raw = purrr::map(raw, parse_targets)) |>
      tidyr::unnest(target_raw, keep_empty = FALSE)

    if (nrow(expanded) == 0) return(tibble::tibble())

    expanded |>
      dplyr::mutate(
        source_id   = trimws(as.character(source_id)),
        target_raw  = trimws(as.character(target_raw)),
        target_type = unname(edge_map[edge_type])
      ) |>
      (\(d) {
        bad <- is.na(d$target_type)
        if (any(bad)) {
          stop(
            "Unknown edge_type(s) in table '", source_tbl, "': ",
            paste(sort(unique(d$edge_type[bad])), collapse = ", "),
            call. = FALSE
          )
        }
        d
      })() |>
      dplyr::mutate(
        child_id = paste0(source_tbl, ":", source_id),
        parent_id = dplyr::if_else(
          is_decorated_id(target_raw),
          target_raw,
          paste0(target_type, ":", target_raw)
        )
      ) |>
      dplyr::transmute(parent_id, child_id, edge_type)
  }) |>
    dplyr::filter(!is.na(parent_id), nzchar(parent_id)) |>
    dplyr::filter(!is.na(child_id),  nzchar(child_id)) |>
    dplyr::distinct()

  # 2) explicit edges from edge sheet (optional)
  edge_sheet_name <- dplyr::case_when(
    "edge"  %in% names(bundle) ~ "edge",
    "edges" %in% names(bundle) ~ "edges",
    TRUE ~ NA_character_
  )


  explicit <- tibble::tibble()

  if (!is.na(edge_sheet_name)) {
    edge_df <- bundle[[edge_sheet_name]]

    if (!is.null(edge_df) && nrow(edge_df) > 0) {

      req <- c("parent_id", "child_id", "edge_type")
      miss <- setdiff(req, names(edge_df))
      if (length(miss)) {
        stop("Edge sheet missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
      }

      explicit <- edge_df |>
        dplyr::mutate(
          parent_id = trimws(as.character(parent_id)),
          child_id  = trimws(as.character(child_id)),
          edge_type = trimws(as.character(edge_type))
        ) |>
        dplyr::filter(
          !is.na(parent_id), nzchar(parent_id),
          !is.na(child_id),  nzchar(child_id),
          !is.na(edge_type), nzchar(edge_type)
        )

      # Decorate IDs if needed using parent_type/child_type columns
      p_ok <- is_decorated_id(explicit$parent_id)
      c_ok <- is_decorated_id(explicit$child_id)

      if (!(all(p_ok) && all(c_ok))) {
        need <- c("parent_type", "child_type")
        if (!all(need %in% names(explicit))) {
          stop(
            "Edge sheet has non-decorated IDs but is missing parent_type/child_type.",
            call. = FALSE
          )
        }

        explicit <- explicit |>
          dplyr::mutate(
            parent_type = trimws(as.character(parent_type)),
            child_type  = trimws(as.character(child_type)),
            parent_id = dplyr::if_else(p_ok, parent_id, paste0(parent_type, ":", parent_id)),
            child_id  = dplyr::if_else(c_ok, child_id,  paste0(child_type,  ":", child_id))
          )
      }

      # Validate edge_type and allowed type pairing using ref_cols rules
      rules <- purrr::imap_dfr(ref_cols, \(edge_map, child_type) {
        tibble::tibble(
          child_type  = child_type,
          edge_type   = names(edge_map),
          parent_type = unname(edge_map)
        )
      })

      explicit_checked <- explicit |>
        dplyr::mutate(
          child_type_actual  = child_type_from_id(child_id),
          parent_type_actual = parent_type_from_id(parent_id)
        ) |>
        dplyr::left_join(rules, by = c("child_type_actual" = "child_type", "edge_type" = "edge_type"))

      bad_edge_type <- explicit_checked |> dplyr::filter(is.na(parent_type))
      if (nrow(bad_edge_type) > 0) {
        ex <- bad_edge_type |> dplyr::slice_head(n = 10)
        stop(
          "Edge sheet contains invalid edge_type for the given child type. Examples:\n",
          paste0("  ", ex$child_id, " --[", ex$edge_type, "]--> ", ex$parent_id, collapse = "\n"),
          call. = FALSE
        )
      }

      bad_parent_type <- explicit_checked |>
        dplyr::filter(parent_type_actual != parent_type)

      if (nrow(bad_parent_type) > 0) {
        ex <- bad_parent_type |> dplyr::slice_head(n = 10)
        stop(
          "Edge sheet contains invalid parent type for the edge rule. Examples:\n",
          paste0(
            "  ", ex$child_id, " --[", ex$edge_type, "]--> ", ex$parent_id,
            " (expected parent_type=", ex$parent_type, ")",
            collapse = "\n"
          ),
          call. = FALSE
        )
      }

      # Keep ALL columns from explicit (so role/evidence/etc. survive),
      # but ensure at least these exist
      explicit <- explicit |>
        dplyr::distinct()

      # (Optional) If you want to forbid explicit edges pointing to unknown tables,
      # you can validate child_type_actual/parent_type_actual are in known object types here.
    }
  }

  # 3) precedence merge: explicit edge sheet overrides inferred
  #    (same parent_id, child_id, edge_type)
  if (nrow(explicit) == 0) return(inferred)

  # Make sure inferred has any extra columns explicit might have (fill with NA)
  extra_cols <- setdiff(names(explicit), names(inferred))
  if (length(extra_cols)) {
    inferred[extra_cols] <- NA
  }
  # And explicit has the inferred columns too (fill with NA) for rbind consistency
  extra_cols2 <- setdiff(names(inferred), names(explicit))
  if (length(extra_cols2)) {
    explicit[extra_cols2] <- NA
  }

  key_inferred <- paste(inferred$parent_id, inferred$child_id, inferred$edge_type, sep = "|")
  key_explicit <- paste(explicit$parent_id, explicit$child_id, explicit$edge_type, sep = "|")

  inferred_keep <- inferred[!key_inferred %in% key_explicit, , drop = FALSE]

  if (!quiet && any(key_inferred %in% key_explicit)) {
    message("Edge sheet overrides ", sum(key_inferred %in% key_explicit), " inferred edge(s).")
  }

  merged <- dplyr::bind_rows(
    explicit,        # precedence
    inferred_keep
  ) |>
    dplyr::distinct()

  merged
}




# ???
`%||%` <- function(x, y) if (is.null(x)) y else x





validate_edge_endpoints_exist <- function(edge_rows, object_rows) {

  db_ids = gopheR::with_gopher_con(function(c) {
    gopheR::gopher_query(c, "
    SELECT object_id FROM object
  ")})$object_id

  stopifnot(is.data.frame(edge_rows), is.data.frame(object_rows))
  stopifnot(all(c("parent_id","child_id","edge_type") %in% names(edge_rows)))
  stopifnot("object_id" %in% names(object_rows))

  bundle_ids <- unique(object_rows$object_id)
  allowed_ids <- if (is.null(db_ids)) bundle_ids else unique(c(bundle_ids, db_ids))

  missing_parent <- setdiff(unique(edge_rows$parent_id), allowed_ids)
  missing_child  <- setdiff(unique(edge_rows$child_id),  allowed_ids)

  if (length(missing_parent) || length(missing_child)) {
    msg <- c("Edge endpoints reference unknown objects:")
    if (length(missing_parent)) msg <- c(msg, paste0("  Missing parent_id: ", paste(missing_parent, collapse = ", ")))
    if (length(missing_child))  msg <- c(msg, paste0("  Missing child_id:  ", paste(missing_child,  collapse = ", ")))
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}





edge_rules_from_ref_cols <- function() {
  purrr::imap_dfr(ref_cols, \(edge_map, child_type) {
    tibble::tibble(
      child_type  = child_type,
      edge_type   = names(edge_map),
      parent_type = unname(edge_map)
    )
  })
}

is_decorated_id <- function(x) grepl("^[^:]+:.+$", x)

split_object_id <- function(x) {
  # returns tibble(type, id) for a vector of decorated ids
  type <- sub(":.*$", "", x)
  id   <- sub("^[^:]+:", "", x)
  tibble::tibble(type = type, id = id)
}

decorate_id <- function(id, type) paste0(type, ":", id)

bundle_edge_sheet_rows <- function(bundle) {
  nm <- names(bundle)
  sheet <- dplyr::case_when(
    "edge"  %in% nm ~ "edge",
    "edges" %in% nm ~ "edges",
    TRUE ~ NA_character_
  )

  if (is.na(sheet)) return(NULL)
  bundle[[sheet]]
}

normalize_edge_sheet <- function(edge_df) {
  if (is.null(edge_df) || nrow(edge_df) == 0) return(tibble::tibble())

  req <- c("parent_id", "child_id", "edge_type")
  miss <- setdiff(req, names(edge_df))
  if (length(miss)) stop("Edge sheet missing columns: ", paste(miss, collapse = ", "), call. = FALSE)

  edge_df |>
    dplyr::mutate(
      parent_id = trimws(as.character(parent_id)),
      child_id  = trimws(as.character(child_id)),
      edge_type = trimws(as.character(edge_type))
    ) |>
    dplyr::filter(
      !is.na(parent_id), nzchar(parent_id),
      !is.na(child_id),  nzchar(child_id),
      !is.na(edge_type), nzchar(edge_type)
    )
}

decorate_edge_sheet_ids <- function(edge_df) {
  # If already decorated, keep. If not, require parent_type/child_type.
  parent_ok <- is_decorated_id(edge_df$parent_id)
  child_ok  <- is_decorated_id(edge_df$child_id)

  if (all(parent_ok) && all(child_ok)) {
    return(edge_df)
  }

  need_cols <- c("parent_type", "child_type")
  if (!all(need_cols %in% names(edge_df))) {
    stop(
      "Edge sheet has non-decorated IDs but is missing parent_type/child_type columns.",
      call. = FALSE
    )
  }

  edge_df |>
    dplyr::mutate(
      parent_type = trimws(as.character(parent_type)),
      child_type  = trimws(as.character(child_type)),
      parent_id = dplyr::if_else(parent_ok, parent_id, decorate_id(parent_id, parent_type)),
      child_id  = dplyr::if_else(child_ok,  child_id,  decorate_id(child_id,  child_type))
    )
}

validate_edges_against_rules <- function(edge_rows, rules) {
  if (nrow(edge_rows) == 0) return(invisible(TRUE))

  child_parts  <- split_object_id(edge_rows$child_id)
  parent_parts <- split_object_id(edge_rows$parent_id)

  checked <- edge_rows |>
    dplyr::mutate(
      child_type  = child_parts$type,
      parent_type = parent_parts$type
    ) |>
    dplyr::left_join(rules, by = c("child_type", "edge_type")) |>
    dplyr::mutate(
      bad_edge_type = is.na(parent_type.y),
      bad_parent_type = !bad_edge_type & parent_type.x != parent_type.y
    )

  bad1 <- checked |> dplyr::filter(bad_edge_type)
  if (nrow(bad1) > 0) {
    stop(
      "Edge sheet contains invalid edge_type for child_type. Examples:\n",
      paste0(
        "  ", bad1$child_id, " --[", bad1$edge_type, "]--> ", bad1$parent_id
      )[seq_len(min(10, nrow(bad1)))],
      collapse = "\n",
      call. = FALSE
    )
  }

  bad2 <- checked |> dplyr::filter(bad_parent_type)
  if (nrow(bad2) > 0) {
    stop(
      "Edge sheet contains invalid parent_type for edge rule. Examples:\n",
      paste0(
        "  ", bad2$child_id, " --[", bad2$edge_type, "]--> ", bad2$parent_id,
        " (expected parent_type=", bad2$parent_type.y, ")"
      )[seq_len(min(10, nrow(bad2)))],
      collapse = "\n",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


merge_edges_with_precedence <- function(inferred, explicit) {
  if (nrow(explicit) == 0) return(inferred)
  if (nrow(inferred) == 0) return(explicit)

  key <- function(df) paste(df$parent_id, df$child_id, df$edge_type, sep = "|")

  inferred$key  <- key(inferred)
  explicit$key  <- key(explicit)

  inferred_keep <- inferred[!inferred$key %in% explicit$key, , drop = FALSE]
  inferred_keep$key <- NULL
  explicit$key <- NULL

  dplyr::bind_rows(explicit, inferred_keep)
}







# internal helper (keep unexported)
insert_aligned <- function(con, table, df) {
  if (!DBI::dbExistsTable(con, table)) {
    stop("DB table not found: ", table, call. = FALSE)
  }
  if (is.null(df) || nrow(df) == 0) return(invisible(0L))

  db_cols <- DBI::dbListFields(con, table)

  # drop any df cols not in db
  df <- df[, intersect(names(df), db_cols), drop = FALSE]

  # add missing db cols
  missing <- setdiff(db_cols, names(df))
  if (length(missing) > 0) {
    for (m in missing) df[[m]] <- NA
  }

  # reorder to db
  df <- df[, db_cols, drop = FALSE]

  DBI::dbWriteTable(
    con, table, df,
    append = TRUE,
    overwrite = FALSE,
    row.names = FALSE
  )

  invisible(nrow(df))
}

#' Insert typed object tables (study/site/sample/readset/assembly/mag)
#'
#' Uses gopheR connection management and drops template-only edge columns
#' defined in `gopher_additional_cols_default`.
#'
#' @param bundle Named list of data frames (from read_bundle()).
#' @param tables Which typed object tables to insert.
#' @param additional_cols Template-only edge columns to drop (package data).
#' @param strict Error if a requested table is missing from bundle.
#'
#' @return Named list of data frames actually inserted (one per table).
#' @export
insert_types <- function(bundle,
                         tables = c("study","site","sample","readset","assembly","mag"),
                         additional_cols = gopher_additional_cols_default,
                         strict = TRUE) {
  stopifnot(is.list(bundle))
  stopifnot(is.character(tables), length(tables) >= 1)

  gopheR::with_gopher_con(function(con) {

    inserted <- list()

    for (tbl in tables) {
      if (!tbl %in% names(bundle)) {
        if (strict) stop("Bundle missing sheet: ", tbl, call. = FALSE)
        next
      }

      df <- bundle[[tbl]]

      # drop template-only edge columns (e.g. sampled_from, binned_from, etc.)
      drop_cols <- additional_cols[[tbl]]
      if (!is.null(drop_cols) && length(drop_cols) > 0) {
        df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
      }

      # detect which columns exist in DB table
      cols <- gopheR::gopher_query(
        con,
        sprintf("PRAGMA table_info(%s)", tbl)
      )$name

      out <- df[, intersect(names(df), cols), drop = FALSE]

      # insert (let DB constraints handle duplicates)
      DBI::dbWriteTable(con, tbl, out, append = TRUE)

      inserted[[tbl]] <- out
    }

    inserted
  })
}





insert_results <- function(bundle,
                           tables,
                           run_tables = NULL,
                           strict = TRUE,
                           validate_object_ids = TRUE,
                           object_id_col = "object_id",
                           objects_table = "object") {
  stopifnot(is.list(bundle))
  stopifnot(is.character(tables), length(tables) >= 1)
  if (!is.null(run_tables)) stopifnot(is.character(run_tables))

  # helper: fill missing/blank UIDs in a specified column using make_object_uid()
  fill_uids <- function(df, uid_col) {
    stopifnot(is.character(uid_col), length(uid_col) == 1)

    # ensure uid column exists
    if (!uid_col %in% names(df)) {
      df[[uid_col]] <- NA_character_
    }

    missing <- is.na(df[[uid_col]]) | trimws(df[[uid_col]]) == ""
    if (!any(missing)) return(df)

    # columns used to construct the fingerprint (exclude uid itself)
    cols_use <- setdiff(names(df), uid_col)
    cols_use <- sort(cols_use)  # stable order

    for (i in which(missing)) {
      vals <- vapply(cols_use, function(col) {
        v <- df[[col]][i]

        if (length(v) == 0 || is.na(v)) return("")
        if (is.numeric(v)) return(format(v, scientific = FALSE, trim = TRUE))
        as.character(v)
      }, character(1))

      # name=value pairs prevent ambiguity
      fingerprint <- paste0(cols_use, "=", vals, collapse = "|")

      df[[uid_col]][i] <- make_object_uid(fingerprint)
    }

    df
  }

  # helper: infer uid column name from table
  infer_uid_col <- function(tbl) {
    if (grepl("_run$", tbl)) "run_id" else paste0(tbl, "_uid")
  }

  gopheR::with_gopher_con(function(con) {

    inserted <- list()

    # Helper: insert a single table using DB columns intersection
    insert_one <- function(tbl, df) {
      # Fill uid column before intersecting cols (so it gets inserted if present in DB)
      uid_col <- infer_uid_col(tbl)
      df <- fill_uids(df, uid_col)

      # detect which columns exist in DB table
      cols <- gopheR::gopher_query(
        con,
        sprintf("PRAGMA table_info(%s)", tbl)
      )$name

      out <- df[, intersect(names(df), cols), drop = FALSE]

      # insert (let DB constraints handle duplicates / FKs)
      DBI::dbWriteTable(con, tbl, out, append = TRUE)

      out
    }

    # 1) Insert run tables first (if any)
    if (!is.null(run_tables) && length(run_tables) > 0) {
      for (tbl in run_tables) {
        if (!tbl %in% names(bundle)) {
          if (strict) stop("Bundle missing sheet: ", tbl, call. = FALSE)
          next
        }
        df <- bundle[[tbl]]
        inserted[[tbl]] <- insert_one(tbl, df)
      }
    }

    # 2) Optionally validate object_id references (generic, table-agnostic)
    if (validate_object_ids) {
      obj_ids <- gopheR::gopher_query(
        con,
        sprintf("SELECT object_id FROM %s", objects_table)
      )$object_id
      obj_set <- unique(obj_ids)

      for (tbl in tables) {
        if (!tbl %in% names(bundle)) {
          if (strict) stop("Bundle missing sheet: ", tbl, call. = FALSE)
          next
        }
        df <- bundle[[tbl]]

        if (!object_id_col %in% names(df)) {
          stop("Result sheet '", tbl, "' is missing required column '",
               object_id_col, "'.", call. = FALSE)
        }

        ids <- unique(df[[object_id_col]])
        ids <- ids[!is.na(ids) & nzchar(trimws(ids))]

        bad <- setdiff(ids, obj_set)
        if (length(bad) > 0) {
          stop(
            "Result sheet '", tbl, "' contains unknown ", object_id_col, "(s): ",
            paste(utils::head(bad, 50), collapse = ", "),
            if (length(bad) > 50) paste0(" ... (", length(bad) - 50, " more)") else "",
            call. = FALSE
          )
        }
      }
    }

    # 3) Insert the result tables
    for (tbl in tables) {
      if (!tbl %in% names(bundle)) {
        if (strict) stop("Bundle missing sheet: ", tbl, call. = FALSE)
        next
      }

      df <- bundle[[tbl]]
      inserted[[tbl]] <- insert_one(tbl, df)
    }

    inserted
  })
}



