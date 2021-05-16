make_names_unique <- function(x) {
  paths <- x$Get("path")
  path_str <- purrr::map_chr(paths, ~paste(.x, collapse = "/"))
  x$Set(short_name = x$Get("name"))
  x$Set(name_root = stringr::str_extract(x$Get("name"), "^[^.]+"))
  x$Set(name = path_str)
  return(x)
}
