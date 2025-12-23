#' Write Excel templates for data entry
#'
#' Creates a folder of Excel templates (one workbook per table) derived from the
#' database schema. Intended for users to fill and return for QC + ingestion.
#'
#' @param out_dir Directory to write templates into.
#' @param tables Character vector of table names. If NULL, uses all DB tables
#'   except internal SQLite tables.
#' @param path,db,read_only Passed to `with_gopher_con()`.
#' @param exclude_cols Column names to omit (system-managed / derived).
#' @param edge_types Optional character vector of allowed `edge_type` values.
#'   Used to add an Excel dropdown validation in the edge template.
#' @param edge_include_helper_cols Logical; if TRUE, adds helper columns to the
#'   edge template (child_type/child_id/parent_type/parent_id) to reduce UID typos.
#' @param n_blank_rows Number of blank rows to pre-allocate in each sheet.
#'
#' @returns Invisibly returns vector of file paths written.
#' @export
write_templates <- function(out_dir,
                            tables = NULL,
                            path = NULL,
                            db = "gopheR_db.sqlite",
                            read_only = TRUE,
                            exclude_cols = c("created_at", "updated_at", "row_hash", "stable_hash"),
                            edge_types = NULL,
                            edge_include_helper_cols = TRUE,
                            n_blank_rows = 50) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  with_gopher_con(function(con) {
    if (is.null(tables)) {
      tables <- DBI::dbListTables(con)
      tables <- tables[!grepl("^sqlite_", tables)]
    }
    
    written <- character(0)
    
    for (tbl in tables) {
      cols <- DBI::dbListFields(con, tbl)
      cols <- setdiff(cols, exclude_cols)
      
      # edge-specific helper columns (optional)
      if (identical(tbl, "edge") && isTRUE(edge_include_helper_cols)) {
        helper <- c("child_type", "child_id", "parent_type", "parent_id")
        # keep canonical columns too; helpers come first to encourage correct entry
        cols <- unique(c(helper, cols))
      }
      
      # Create a blank data.frame with headers and preallocated blank rows
      df <- as.data.frame(setNames(replicate(length(cols), NA, simplify = FALSE), cols))
      df <- df[rep(1, n_blank_rows), , drop = FALSE]
      df[,] <- NA
      
      file <- file.path(out_dir, paste0(tbl, ".xlsx"))
      
      wb <- openxlsx::createWorkbook()
      
      # Main sheet
      openxlsx::addWorksheet(wb, sheetName = tbl)
      openxlsx::writeData(wb, sheet = tbl, x = df, withFilter = TRUE)
      openxlsx::freezePane(wb, sheet = tbl, firstRow = TRUE)
      
      # Style header
      openxlsx::addStyle(
        wb, tbl,
        style = openxlsx::createStyle(textDecoration = "bold"),
        rows = 1, cols = seq_along(cols),
        gridExpand = TRUE
      )
      
      # Edge-specific enhancements
      if (identical(tbl, "edge")) {
        # Instructions sheet
        openxlsx::addWorksheet(wb, "INSTRUCTIONS")
        instructions <- data.frame(
          section = c("Purpose", "UID format", "Helper columns", "Edge types", "Common mistakes"),
          text = c(
            "Edges connect existing and/or newly provided objects. You may reference objects already in the database.",
            "Use typed UIDs: mag:M001, assembly:A014, read_set:R123, sample:S45 (prefix + ':' + id).",
            "If helper columns are present (child_type/child_id/etc.), fill those and leave child_uid/parent_uid blank; ingestion can construct UIDs.",
            if (!is.null(edge_types)) paste("Allowed edge_type values:", paste(edge_types, collapse = ", ")) else "Define allowed edge_type values in your project settings if you want dropdown validation.",
            "Typos in prefixes (mag vs MAG), missing ':' delimiter, and extra whitespace are the most common issues."
          ),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "INSTRUCTIONS", instructions, colNames = TRUE)
        openxlsx::setColWidths(wb, "INSTRUCTIONS", cols = 1:2, widths = c(20, 120))
        
        # Dropdown validation for edge_type (if provided and column exists)
        if (!is.null(edge_types) && "edge_type" %in% cols) {
          # Create a hidden sheet with allowed edge types
          openxlsx::addWorksheet(wb, "._lists")
          openxlsx::writeData(wb, "._lists", x = data.frame(edge_type = edge_types))
          openxlsx::hideWorksheet(wb, "._lists")
          
          # Apply validation to edge_type column for the data rows (2..n_blank_rows+1)
          edge_type_col <- which(cols == "edge_type")
          data_rows <- 2:(n_blank_rows + 1)
          
          openxlsx::dataValidation(
            wb, sheet = tbl,
            cols = edge_type_col, rows = data_rows,
            type = "list",
            value = sprintf("._lists!$A$2:$A$%d", length(edge_types) + 1),
            allowBlank = TRUE,
            showInputMsg = TRUE,
            promptTitle = "edge_type",
            prompt = "Choose an allowed edge_type value."
          )
        }
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      written <- c(written, file)
    }
    
    # Bundle README
    readme <- file.path(out_dir, "README.txt")
    msg <- c(
      "Template bundle for gopheR ingestion",
      "",
      "Fill the .xlsx files and return them for QC + ingestion.",
      "One workbook per database table.",
      "",
      "Notes:",
      "- Do not change column names.",
      "- Leave system-managed columns out (they are excluded by default).",
      "- Edges may reference objects already in the DB using typed UIDs."
    )
    writeLines(msg, readme)
    
    invisible(written)
  }, path = path, db = db, read_only = read_only)
}