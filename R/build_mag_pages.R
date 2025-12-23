suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(here)
  library(glue)
  library(fs)
  library(readr)
  library(stringr)
  library(gopheR)
})

db_file       <- gopher_db_path(path = here::here("data"))
template_file <- here::here("mag", "_template_mag.qmd")
out_dir       <- here::here("mag")

stopifnot(file.exists(db_file))
stopifnot(file.exists(template_file))

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)
on.exit(DBI::dbDisconnect(con), add = TRUE)

# Real validity check (better than dbIsValid)
DBI::dbGetQuery(con, "SELECT 1")

mags <- DBI::dbGetQuery(con, "SELECT mag_id FROM mag ORDER BY mag_id")

fs::dir_create(out_dir)
template <- readr::read_file(template_file)

for (id in mags$mag_id) {
  out <- stringr::str_replace_all(
    template,
    stringr::fixed("{{< meta mag_id >}}"),
    id
  )
  out <- stringr::str_replace(out, 'mag_id: "M001"', glue::glue('mag_id: "{id}"'))
  readr::write_file(out, here::here("mag", glue::glue("{id}.qmd")))
}

message("Generated ", nrow(mags), " MAG pages")
