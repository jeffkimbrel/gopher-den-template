read_bundle <- function(path, tables = NULL) {
  path <- path.expand(path)

  if (is.null(tables)) {
    tables <- openxlsx::getSheetNames(path)
  }

  tables |>
    purrr::set_names() |>
    purrr::map(\(tbl) {
      openxlsx::read.xlsx(path, sheet = tbl) |>
        tibble::as_tibble() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), \(x) {
          if (is.character(x)) stringr::str_squish(dplyr::na_if(x, "")) else x
        }))
    })
}
