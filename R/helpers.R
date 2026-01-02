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
