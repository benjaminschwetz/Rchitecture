#' Title
#'
#' @param project_path absolute path pointing to the files
#' @param include_folders should folders be included?
#' @param path_prefix leading part of the absolute file paths that is not meaningful for the analysis
#' @param .ignore_regex regex pattern for paths to exclude
#'
#' @return table
#' @export
#'
#' @examples
content_table <- function(project_path , include_folders, path_prefix, .ignore_regex=NULL) {
  #requires a little hack to stay pretty both with relative paths and system.file()
  data <- file.info(list.files(project_path, recursive = TRUE, full.names = TRUE, include.dirs = include_folders))[,1:2]
  tbl_data <- tibble::rownames_to_column(data, "path")
  out <- tbl_data %>%
    dplyr::mutate(
      path = stringr::str_remove(path, paste0("^", path_prefix, "/")),
      file_name = stringr::str_extract(path, "(?<=/)[^/]+$"),
      file_type = dplyr::case_when(
        isdir ~ NA_character_,
        TRUE ~ stringr::str_extract(file_name, "[^.]*$")
    )
    )
  if(!is.null(.ignore_regex)){
    out <- out %>%
      rowwise() %>%
      filter(!any(stringr::str_detect(path, .ignore_regex)))
  }
  return(out)
}
