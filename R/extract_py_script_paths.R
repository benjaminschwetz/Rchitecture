extract_py_script_paths <- function(folder){
  files <- list.files(folder, recursive = TRUE, full.names = TRUE) %>%
    stringr::str_extract(".*\\.py$") %>%
    na.omit()
  return(files)
}
