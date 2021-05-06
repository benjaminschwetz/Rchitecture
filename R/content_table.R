#' Title
#'
#' @param project_path
#'
#' @return table
#' @export
#'
#' @examples
content_table <- function(project_path = system.file("example_data/Zeeguu-Ecosystem/", package="Rchitecture")) {
  #requires a little hack to stay pretty both with relative paths and system.file()
  data <- file.info(list.files(project_path, recursive = TRUE, full.names = TRUE, include.dirs = TRUE))[,1:2]
  tbl_data <- tibble::rownames_to_column(data, "path")
  out <- tbl_data %>%
    dplyr::mutate(
      file_name = stringr::str_extract(path, "(?<=/)[^/]+$"),
      file_type = dplyr::case_when(
        isdir ~ NA_character_,
        TRUE ~ stringr::str_extract(file_name, "[^.]*$")
    )
    )
  return(out)
}
